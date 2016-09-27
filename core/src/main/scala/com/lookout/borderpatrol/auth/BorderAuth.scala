package com.lookout.borderpatrol.auth

import com.lookout.borderpatrol.{BpCommunicationError, BpCoreError, BpNotFoundRequest}
import com.lookout.borderpatrol.{CustomerIdentifier, ServiceIdentifier, ServiceMatcher}
import com.lookout.borderpatrol.util.Combinators._
import com.lookout.borderpatrol.util.Helpers
import com.lookout.borderpatrol.sessionx._
import com.twitter.finagle.http.path.{Path, Root}
import com.twitter.finagle.http._
import com.twitter.finagle.stats.StatsReceiver
import com.twitter.finagle.{Filter, Service, SimpleFilter}
import com.twitter.logging.Logger
import com.twitter.logging.config.Level
import com.twitter.util.Future
import io.circe.Json
import io.circe.syntax._
import org.jboss.netty.handler.codec.http.QueryStringDecoder
import scala.util.{Failure, Success, Try}


/**
 * PODs
 */
case class CustomerIdRequest(req: Request, customerId: CustomerIdentifier)
case class SessionIdRequest(req: Request, customerId: CustomerIdentifier, serviceIdOpt: Option[ServiceIdentifier],
                            sessionIdOpt: Option[SignedId])
object SessionIdRequest {
  def apply(sr: CustomerIdRequest, serviceIdOpt: Option[ServiceIdentifier],
            sidOpt: Option[SignedId]): SessionIdRequest =
    SessionIdRequest(sr.req, sr.customerId, serviceIdOpt, sidOpt)
}
case class BorderRequest(req: Request, customerId: CustomerIdentifier, serviceId: ServiceIdentifier,
                         sessionId: SignedId)
object BorderRequest {
  def apply(sr: SessionIdRequest, serviceId: ServiceIdentifier, sessionId: SignedId): BorderRequest =
    BorderRequest(sr.req, sr.customerId, serviceId, sessionId)
}
case class AccessIdRequest[A](req: Request, customerId: CustomerIdentifier, serviceId: ServiceIdentifier,
                              sessionId: SignedId, id: Id[A])
object AccessIdRequest {
  def apply[A](sr: BorderRequest, id: Id[A]): AccessIdRequest[A] =
    AccessIdRequest(sr.req, sr.customerId, sr.serviceId, sr.sessionId, id)
}

object BorderAuth {
  private[this] val log = Logger.get(getClass.getPackage.getName)

  def expectsJson(req: Request): Boolean = {
    val decoder = Try(new QueryStringDecoder(req.uri)).toOption
    decoder.exists(_.getPath.endsWith(".json")) ||
      req.headerMap.get("Accept").exists(_.contains(MediaType.Json))
  }

  /* Convert a redirect into a response palatable to the client */
  def formatRedirectResponse(req: Request, status: Status, location: String, sessionIdOpt: Option[SignedId],
                             msg: String, msgSrc: String = "borderpatrol"): Response = {
    log.info(msg)
    tap(Response())(res => {
      sessionIdOpt.foreach(sessionId => res.addCookie(sessionId.asCookie()))
      expectsJson(req) match {
        case true =>
          res.status = status
          res.contentString = Json.fromFields(Seq(
            ("msg_source", msgSrc.asJson),
            ("redirect_url", location.asJson))).toString()
          res.setContentTypeJson()
        case _ =>
          // Change the Status to be 302
          res.status = Status.Found
          res.location = location
          res.contentType = "text/plain"
      }})
  }

  /* Convert this error into a response appropriate to the client */
  def formatLogoutResponse(req: Request, status: Status, location: String, msg: String): Response = {
    log.info(msg)
    tap(Response())(res => {
      // Expire all BP cookies present in the Request
      req.cookies.foreach[Unit] {
        case (name: String, cookie: Cookie) if name.startsWith("border_") =>
          res.addCookie(SignedId.toExpiredCookie(name))
        case _ =>
      }
      expectsJson(req) match {
        case true =>
          res.status = status
          res.contentString = Json.fromFields(Seq(
            ("msg_source", "borderpatrol".asJson),
            ("description", "Logged out successfully".asJson),
            ("redirect_url", location.asJson))).toString()
          res.setContentTypeJson()
        case _ =>
          res.status = Status.Found
          res.location = location
          res.contentType = "text/plain"
          res.contentString = "Logged out successfully"
      }})
  }

  /**
    * This filter rewrites Request Path as per the ServiceIdentifier configuration
    */
  def rewriteRequest(req: Request, serviceId: ServiceIdentifier): Request =
    serviceId.rewritePath.fold(req) { rwPath =>
      tap(req) {r =>
        /*
         * After rewrite, we need to ensure that rewritten path is NOT empty (becuase Path.Root is represented by emtpy
         * string. Thus, if the rewritten path is going to be Root (i.e. empty), then use "/" as the rwPath
         */
        r.uri = if (r.path.replaceFirst(serviceId.path.toString, rwPath.toString).isEmpty)
          r.uri.replaceFirst(serviceId.path.toString, "/")
        else r.uri.replaceFirst(serviceId.path.toString, rwPath.toString)
      }
    }

  /**
    * Centralize attempt to fetch a 'destination' param. In future this could be parametrized.
    * Try to get a scrubbed value that then needs to be checked against known host names.
    *
    * @param paramMap
    * @param destinationValidator
    * @return Option if a valid hostname (value) was extracted from paramMap
    */
  def extractDestination(paramMap: ParamMap)(implicit destinationValidator: DestinationValidator): Option[String] =
    for {
      loc <- Helpers.scrubQueryParams(paramMap, "destination")
      a <- destinationValidator.checkHosts(loc)
    } yield a

  /**
    * Allocate new session, store the request in cache and return the signedId
    *
    * @param req
    * @param store
    * @return
    */
  def allocateUntaggedSession(req: Request, store: SessionStore)
                             (implicit secretStore: SecretStoreApi): Future[SignedId] = {
    for {
      session <- Session(req)
      _ <- store.update(session)
    } yield session.id
  }
}

/**
 * Determines the service that the request is trying to contact
 * If the service doesn't exist, it returns a 404 Not Found response
 *
 * @param matcher
 */
case class CustomerIdFilter(matcher: ServiceMatcher)(implicit statsReceiver: StatsReceiver)
    extends Filter[Request, Response, CustomerIdRequest, Response] {
  private[this] val log = Logger.get(getClass.getPackage.getName)

  def apply(req: Request, service: Service[CustomerIdRequest, Response]): Future[Response] = {
    for {
      custIdOpt <- Future.value(req.host.flatMap(matcher.customerId))
      resp <- custIdOpt match {
        case None => Future.exception(BpNotFoundRequest("Failed to find CustomerId for " +
          s"Request(${req.method}, ${req.host.fold("null-hostname")(h => s"$h${req.path}")})"))
        case Some(cid) =>
          log.debug(s"Processing: Request(${req.method}, ${req.host.get}${req.path} " +
            s"with CustomerId: ${cid.subdomain}")
          service(CustomerIdRequest(req, cid))
      }
    } yield resp
  }
}

/**
 * Ensures we have a SignedId present in this request, sending a Redirect to the service login page if it doesn't
 */
case class SessionIdFilter(matcher: ServiceMatcher, store: SessionStore)(
  implicit secretStore: SecretStoreApi, statsReceiver: StatsReceiver)
    extends Filter[CustomerIdRequest, Response, SessionIdRequest, Response] {
  private[this] val log = Logger.get(getClass.getPackage.getName)
  private[this] val statSessionNotFound = statsReceiver.counter("req.session.id.notfound")

  /**
   * Passes the SignedId to the next in the filter chain. If any failures decoding the SignedId occur
   * (expired, not there, etc), we will terminate early and send a redirect
   *
   * @param req
   * @param service
   */
  def apply(req: CustomerIdRequest, service: Service[SessionIdRequest, Response]): Future[Response] = {
    for {
      sessionIdOpt <- Future.value(SignedId.fromRequest(req.req, SignedId.sessionIdCookieName) match {
        case Success(s) => Some(s)
        case Failure(e) =>
          statSessionNotFound.incr()
          log.debug(s"Did not find the SessionId in ${req.req}, reason: ${e.getMessage}")
          None
      })
      serviceIdOpt <- Future.value(matcher.serviceId(Path(req.req.path)))
      resp <- service(SessionIdRequest(req, serviceIdOpt, sessionIdOpt))
    } yield resp
  }
}

/**
 * Send the request on IdentityProvider chain
 *
 * @param identityProviderMap
 * @param store
 * @param secretStore
 */
case class SendToIdentityProvider(identityProviderMap: Map[String, Service[BorderRequest, Response]],
                                  store: SessionStore)(
  implicit destinationValidator: DestinationValidator, secretStore: SecretStoreApi, statsReceiver: StatsReceiver)
    extends SimpleFilter[SessionIdRequest, Response] {
  private[this] val log = Logger.get(getClass.getPackage.getName)
  private[this] val statLoginRedirects = statsReceiver.counter("req.login.required.redirects")
  private[this] val statIdentityProvider = statsReceiver.counter("req.identity.provider.forwards")

  def sendToIdentityProvider(req: BorderRequest): Future[Response] = {
    log.debug(s"Send: Request(${req.req.method} ${req.req.path}) with SessionId: ${req.sessionId.toLogIdString}, " +
      s"IPAddress: '${req.req.xForwardedFor.getOrElse("No IP Address")}', " +
      s"to identity provider chain for service: ${req.serviceId.name}")
    identityProviderMap.get(req.customerId.loginManager.tyfe) match {
      case Some(ip) => statIdentityProvider.incr(); ip(req)
      case None => Future.exception(BpIdentityProviderError(
        s"Failed to find IdentityProvider Service Chain for loginManager type: ${req.customerId.loginManager.tyfe}, " +
          s"SessionID: ${req.sessionId.toLogIdString}, " +
          s"IPAddress: '${req.req.xForwardedFor.getOrElse("No IP Address")}'"))
    }
  }

  /**
    * Redirect to login page
    */
  def redirectToLogin(req: SessionIdRequest, sessionIdOpt: Option[SignedId]): Future[Response] = {
    for {
      location <- req.customerId.loginManager.redirectLocation(req.req).toFuture
      sessionId <- sessionIdOpt match {
        case Some(sessionId) => sessionId.toFuture
        case None => BorderAuth.allocateUntaggedSession(req.req, store)
      }
    } yield {
      statLoginRedirects.incr()
      BorderAuth.formatRedirectResponse(req.req, Status.Unauthorized, location, Some(sessionId),
        s"Redirecting the ${req.req} with Untagged SessionId: ${sessionId.toLogIdString}, " +
          s"IPAddress: '${req.req.xForwardedFor.getOrElse("No IP Address")}', " +
          s"to login service, location: ${location.split('?').headOption}")
    }
  }

  /**
    * Allocate a new session and send to identity provider
    */
  def allocSessionIdAndSendToIdentityProvider(req: SessionIdRequest): Future[Response] = {
    val location = BorderAuth.extractDestination(req.req.params)(destinationValidator) match {
      case Some(s) => s
      case _ => {
        log.info(s"Could not determine value for query param 'destination': ${req.req.params.get("destination")} " +
          s"while processing the signin request, using default")
        req.customerId.defaultServiceId.path.toString
      }
    }
    for {
      sessionId <- BorderAuth.allocateUntaggedSession(Request(location), store)
      resp <- sendToIdentityProvider(BorderRequest(req, req.customerId.defaultServiceId, sessionId))
    } yield resp
  }

  def apply(req: SessionIdRequest, service: Service[SessionIdRequest, Response]): Future[Response] = {
    (req.sessionIdOpt.map(_.tag), req.serviceIdOpt) match {

      /* 1. loginConfirm path w/ untagged SessionId, dispatch to Identity Provider chain */
      case (Some(Untagged), _)
        if Path(req.req.path).startsWith(req.customerId.loginManager.loginConfirm) => {
        sendToIdentityProvider(BorderRequest(req, req.customerId.defaultServiceId, req.sessionIdOpt.get))
      }

      /**
        * 2. loginConfirm path w/ Authenticated SessionId, dispatch to Identity Provider chain.
        *    Not very intuitive, but apparently there are cases (automated scripts) wants to authenticate
        *    an authenticated session
        */
      case (Some(AuthenticatedTag), _)
        if Path(req.req.path).startsWith(req.customerId.loginManager.loginConfirm) => {
        sendToIdentityProvider(BorderRequest(req, req.customerId.defaultServiceId, req.sessionIdOpt.get))
      }

      /**
        * 3. POST to loginConfirm path w/o SessionId, allocate SessionId, dispatch to Identity Provider chain
        *    It creates default request on the fly and for location it uses defaultServiceId or destination param
        */
      case (None, _)
        if Path(req.req.path).startsWith(req.customerId.loginManager.loginConfirm) => {
        allocSessionIdAndSendToIdentityProvider(req)
      }

      /* 4. Request w/ untagged SessionId, protected path on Service, redirect it to Login */
      case (Some(Untagged), Some(serviceId)) if !serviceId.isUnprotected(req.req.path) =>
        redirectToLogin(req, req.sessionIdOpt)

      /* 5. Request w/ untagged SessionId, no Service, redirect it to Login */
      case (Some(Untagged), None) => redirectToLogin(req, req.sessionIdOpt)

      /* 6. Request w/o SessionId, protected path on Service, redirect it to Login */
      case (None, Some(serviceId)) if !serviceId.isUnprotected(req.req.path) => redirectToLogin(req, None)

      /* 7. Request w/o SessionId, no Service, redirect it to Login */
      case (None, None) => redirectToLogin(req, None)

      /* Everything else */
      case _ => service(req)
    }
  }
}

/**
 * Send the request on AccessIssuer chain
 *
 * This filter only deals with Authenticated SessionIds or forwards it to next filter
 *
 * @param accessIssuerMap
 */
case class SendToAccessIssuer(accessIssuerMap: Map[String, Service[BorderRequest, Response]])(
  implicit statsReceiver: StatsReceiver)
    extends SimpleFilter[SessionIdRequest, Response] {
  private[this] val log = Logger.get(getClass.getPackage.getName)
  private[this] val statAccessIssuer = statsReceiver.counter("req.access.issuer.forwards")

  def sendToAccessIssuer(req: BorderRequest): Future[Response] = {
    log.debug(s"Send: Request(${req.req.method} ${req.req.path}) with SessionId: ${req.sessionId.toLogIdString}, " +
      s"IPAddress: '${req.req.xForwardedFor.getOrElse("No IP Address")}', " +
      s"to access issuer chain for service: ${req.serviceId.name}")
    accessIssuerMap.get(req.customerId.loginManager.tyfe) match {
      case Some(ai) => statAccessIssuer.incr(); ai(req).map { resp =>
        log.debug(s"For a ${req.req} with SessionId: ${req.sessionId.toLogIdString}, " +
          s"IPAddress: '${req.req.xForwardedFor.getOrElse("No IP Address")}', " +
          s"the upstream service (protected path) responded with " +
          s"Response(${resp.status} ContentLength(${resp.contentLength.getOrElse(0)}))")
        resp
      }
      case None => Future.exception(BpAccessIssuerError(
        s"Failed to find AccessIssuer Service Chain for loginManager type: ${req.customerId.loginManager.tyfe}, " +
          s"SessionId: ${req.sessionId.toLogIdString}, " +
          s"IPAddress: '${req.req.xForwardedFor.getOrElse("No IP Address")}'"))
    }
  }

  def apply(req: SessionIdRequest, service: Service[SessionIdRequest, Response]): Future[Response] = {
    (req.sessionIdOpt.map(_.tag), req.serviceIdOpt) match {

      /* 1. Request to protected path w/ authenticated Sessionid, dispatch via accessIssuer chain */
      case (Some(AuthenticatedTag), Some(serviceId)) if !serviceId.isUnprotected(req.req.path) =>
        sendToAccessIssuer(BorderRequest(req, serviceId, req.sessionIdOpt.get))

      /* 2. Request for Root w/ authenticated Sessionid, redirect to default service */
      case (Some(AuthenticatedTag), None) if Root.startsWith(Path(req.req.path)) =>
        BorderAuth.formatRedirectResponse(req.req, Status.NotFound, req.customerId.defaultServiceId.path.toString,
          req.sessionIdOpt,
          s"Redirecting the ${req.req} with Authenticated SessionId: ${req.sessionIdOpt.get.toLogIdString}, " +
            s"IPAddress: '${req.req.xForwardedFor.getOrElse("No IP Address")}', " +
            s"to upstream service, location: ${req.customerId.defaultServiceId.path}").toFuture

      /* Everything else */
      case _ => service(req)
    }
  }
}

/**
 * Send the request to Unprotected path on upstream Service
 *
 * This filter only deals with the Request has SessionId (Authenticate or Untagged) and destined to unprotected path.
 * Everything else is forwarded to the next filter in the chain.
 *
 * @param store
 * @param secretStore
 * @param statsReceiver
 */
case class SendToUnprotectedService(store: SessionStore)
                                   (implicit secretStore: SecretStoreApi, statsReceiver: StatsReceiver)
    extends SimpleFilter[SessionIdRequest, Response] {
  private[this] val log = Logger.get(getClass.getPackage.getName)
  private[this] val statRequestSends = statsReceiver.counter("req.unprotected.upstream.service.forwards")

  def sendToUnprotectedService(req: BorderRequest): Future[Response] = {
    statRequestSends.incr
    log.debug(s"Send: Request(${req.req.method} ${req.req.path}) with SessionId: ${req.sessionId.toLogIdString}, " +
      s"IPAddress: '${req.req.xForwardedFor.getOrElse("No IP Address")}', " +
      s"to the unprotected path on upstream service: ${req.serviceId.name}")
    /* Rewrite Path and send to upstream */
    req.serviceId.endpoint.send(BorderAuth.rewriteRequest(req.req, req.serviceId)).map {resp =>
      log.debug(s"For a ${req.req} with SessionId: ${req.sessionId.toLogIdString}, " +
        s"IPAddress: '${req.req.xForwardedFor.getOrElse("No IP Address")}', " +
        s"the upstream service (unprotected path) responded with " +
        s"Response(${resp.status} ContentLength(${resp.contentLength.getOrElse(0)}))")
      resp
    }
  }

  def apply(req: SessionIdRequest, service: Service[SessionIdRequest, Response]): Future[Response] =
    (req.sessionIdOpt, req.serviceIdOpt) match {

      /* 1. Request w/ SessionId, dispatch to unprotected path on upstream service */
      case (Some(sessionId), Some(serviceId)) if serviceId.isUnprotected(req.req.path) =>
        sendToUnprotectedService(BorderRequest(req, serviceId, sessionId))

      /* 1. Request w/o SessionId, dispatch to unprotected path on upstream service */
      case (None, Some(serviceId)) if serviceId.isUnprotected(req.req.path) =>
        for {
          sessionId <- BorderAuth.allocateUntaggedSession(Request(req.customerId.defaultServiceId.path.toString), store)
          resp <- sendToUnprotectedService(BorderRequest(req, serviceId, sessionId))
          _ <- resp.addCookie(sessionId.asCookie()).toFuture
        } yield resp

      /* Everything else */
      case _ => service(req)
    }
}

/**
 * Logout Service
 * - Deletes the session
 * - sets the empty cookie in response
 * - redirects to default service path
 */
case class LogoutService(store: SessionStore)(implicit destinationValidator: DestinationValidator,
                        secretStore: SecretStoreApi)
    extends Service[SessionIdRequest, Response] {
  private[this] val log = Logger.get(getClass.getPackage.getName)

  def apply(req: SessionIdRequest): Future[Response] = {
    req.sessionIdOpt.foreach { sid =>
      log.debug(s"Logging out Session: ${sid.toLogIdString}")
      store.delete(sid)
    }

    /**
      * Redirect in following order
      * (1) url/path in query param
      * (2) configured logged out url
      * (3) default service path
      */
    val location: String = (BorderAuth.extractDestination(req.req.params),
      req.customerId.loginManager.loggedOutUrl) match {
      case (Some(loc), _) => loc
      case (None, Some(loc)) => loc.toString
      case _ => req.customerId.defaultServiceId.path.toString
    }
    BorderAuth.formatLogoutResponse(req.req, Status.Ok, location,
      s"After logout, redirecting to: '${location}'").toFuture
  }
}

/**
 * Determines the identity of the requester, if no identity it responds with a redirect to the login page for that
 * service
 */
case class IdentityFilter[A : SessionDataEncoder](store: SessionStore)(
  implicit secretStore: SecretStoreApi, statsReceiver: StatsReceiver)
    extends Filter[BorderRequest, Response, AccessIdRequest[A], Response] {
  private[this] val log = Logger.get(getClass.getPackage.getName)

  def identity(sessionId: SignedId): Future[Identity[A]] =
    (for {
      sessionMaybe <- store.get[A](sessionId)
    } yield sessionMaybe.fold[Identity[A]](EmptyIdentity)(s => Id(s.data))) handle {
      case e =>
        log.warning(s"Failed to retrieve Identity with SessionId: ${sessionId.toLogIdString} " +
          s"from sessionStore with: ${e.getMessage}")
        EmptyIdentity
    }

  def apply(req: BorderRequest, service: Service[AccessIdRequest[A], Response]): Future[Response] = {
    identity(req.sessionId).flatMap {
      case id: Id[A] => service(AccessIdRequest(req, id))
      case EmptyIdentity => BorderAuth.allocateUntaggedSession(req.req, store).map { sessionId =>
        val location = req.customerId.loginManager.redirectLocation(req.req)
        BorderAuth.formatRedirectResponse(req.req, Status.Unauthorized, location, Some(sessionId),
          s"Failed to find SessionId: ${req.sessionId.toLogIdString}, " +
            s"IPAddress: '${req.req.xForwardedFor.getOrElse("No IP Address")}', " +
            s" for: ${req.req}, " +
            s"allocating a new SessionId: ${sessionId.toLogIdString}, " +
            s"redirecting to location: ${location.split('?').headOption}")
      }
    }
  }
}

/**
 * This filter acquires the access and then forwards the request to upstream service
 */
case class AccessFilter[A, B](implicit statsReceiver: StatsReceiver)
    extends Filter[AccessIdRequest[A], Response, AccessRequest[A], AccessResponse[B]] {
  private[this] val log = Logger.get(getClass.getPackage.getName)
  private[this] val statRequestSends = statsReceiver.counter("req.upstream.service.request.forwards")

  def apply(req: AccessIdRequest[A],
            accessService: Service[AccessRequest[A], AccessResponse[B]]): Future[Response] = {
    for {
      accessResp <- accessService(AccessRequest(req.id, req.customerId, req.serviceId, req.sessionId))
      resp <- req.serviceId.endpoint.send(
        /* Rewrite Path */
        tap(BorderAuth.rewriteRequest(req.req, req.serviceId)) { r =>
          statRequestSends.incr
          log.debug(s"Send: ${r} with SessionId: ${req.sessionId.toLogIdString}, " +
            s"IPAddress: '${req.req.xForwardedFor.getOrElse("No IP Address")}', " +
            s"to the protected path on upstream service: ${req.serviceId.name}")
          r.headerMap.add("Auth-Token", accessResp.access.access.toString)
        }
      )
    } yield resp
  }
}

/**
 * Top level filter that maps exceptions into appropriate status codes
 */
case class ExceptionFilter() extends SimpleFilter[Request, Response] {
  private[this] val log = Logger.get(getClass.getPackage.getName)

  private[this] def logAndResponse(req: Request, msg: String, status: Status, level: Level): Response = {
    log.log(level, s"BP Exception: $msg")
    tap(Response(status))(res => {
      BorderAuth.expectsJson(req) match {
        case true =>
          res.contentString = Json.fromFields(Seq(
            ("msg_source", "borderpatrol".asJson),
            ("description", "Oops, something went wrong, please try your action again".asJson)
          )).toString()
          res.contentType = "application/json"
        case _ =>
          res.contentType = "text/plain"
          res.contentString = "Oops, something went wrong, please try your action again"
      }
    })
  }

  /**
   * Tells the service how to handle certain types of servable errors (i.e. PetstoreError)
   */
  def errorHandler(req: Request): PartialFunction[Throwable, Response] = {
    case error: BpUserError => logAndResponse(req, error.getMessage, error.status, Level.INFO)
    case error: BpCommunicationError => logAndResponse(req, error.getMessage, error.status, Level.WARNING)
    case error: BpCoreError => logAndResponse(req, error.getMessage, error.status, Level.INFO)
    case error: BpForbiddenRequest => logAndResponse(req, error.getMessage, Status.Forbidden, Level.INFO)
    case error: Throwable => logAndResponse(req, s"${error.getClass.getSimpleName}: ${error.getMessage}",
      Status.InternalServerError, Level.WARNING)
  }

  def apply(req: Request, service: Service[Request, Response]): Future[Response] = {
    service(req) handle errorHandler(req)
  }
}

/**
  * Options Filter
  * - Intercept Requests with OPTIONS method and respond with 200 if service is found or 405 otherwise
  * - For all other methods pass it thru to next filter
  */
case class OptionsFilter(store: SessionStore)(implicit secretStore: SecretStoreApi)
    extends SimpleFilter[SessionIdRequest, Response] {
  private[this] val log = Logger.get(getClass.getPackage.getName)

  def apply(req: SessionIdRequest, service: Service[SessionIdRequest, Response]): Future[Response] = {
    req.req.method match {
      case Method.Options => {
        for {
          sessionId <- req.sessionIdOpt.fold(BorderAuth.allocateUntaggedSession(req.req, store))(_.toFuture)
          resp <- tap(Response()) { r =>
            r.status = req.serviceIdOpt.fold(Status.MethodNotAllowed)(_ => Status.Ok)
            r.addCookie(sessionId.asCookie())
            r.contentLength = 0
          }.toFuture
        } yield resp
      }
      case _ => service(req)
    }
  }
}
