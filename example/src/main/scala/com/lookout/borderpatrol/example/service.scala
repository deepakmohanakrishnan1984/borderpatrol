/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Lookout, Inc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package com.lookout.borderpatrol.example

import java.net.URL

import com.lookout.borderpatrol.auth.tokenmaster.LoginManagers.{BasicLoginManager, OAuth2LoginManager}
import com.lookout.borderpatrol.security.HostHeaderFilter
import com.lookout.borderpatrol.{HealthCheckRegistry, ServiceMatcher}
import com.lookout.borderpatrol.auth._
import com.lookout.borderpatrol.auth.tokenmaster.Tokenmaster._
import com.lookout.borderpatrol.auth.tokenmaster._
import com.lookout.borderpatrol.auth.tokenmaster.Tokens._
import com.lookout.borderpatrol.server.{AccessLogFilter, HealthCheckService}
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.util.Combinators._
import com.twitter.finagle.http.path.Path
import com.twitter.finagle.stats.StatsReceiver
import com.twitter.io.Buf
import com.twitter.finagle.http.{Method, Request, Response, Status}
import com.twitter.finagle.http.service.RoutingService
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.Future
import io.circe._

import scala.util.{Failure, Success, Try}


object service {
  /**
   * Get IdentityProvider map of name -> Service chain
   *
   * As of now, we only support `tokenmaster` as an Identity Provider
   */
  def identityProviderChainMap(sessionStore: SessionStore)(
    implicit store: SecretStoreApi, statsReceiver: StatsReceiver):
  Map[String, Service[BorderRequest, Response]] = Map(
    "tokenmaster.basic" -> tokenmasterBasicServiceChain(sessionStore),
    "tokenmaster.oauth2" -> tokenmasterOAuth2ServiceChain(sessionStore)
  )

  /**
   * Get AccessIssuer map of name -> Service chain
   *
   * As of now, we only support `tokenmaster` as an Access Issuer
   */
  def accessIssuerChainMap(sessionStore: SessionStore)(
    implicit store: SecretStoreApi, statsReceiver: StatsReceiver):
  Map[String, Service[BorderRequest, Response]] = Map(
    "tokenmaster.basic" -> tokenmasterAccessIssuerChain(sessionStore),
    "tokenmaster.oauth2" -> tokenmasterAccessIssuerChain(sessionStore)
  )

  /**
   * The sole entry point for all service chains
   */
  def MainServiceChain(implicit config: ServerConfig, statsReceiver: StatsReceiver, registry: HealthCheckRegistry,
                       secretStore: SecretStoreApi): Service[Request, Response] = {
    val serviceMatcher = ServiceMatcher(config.customerIdentifiers, config.serviceIdentifiers)
    val notFoundService = Service.mk[SessionIdRequest, Response] { req => Response(Status.NotFound).toFuture }
    implicit val destinationValidator = DestinationValidator(config.allowedDomains)
    val serviceChainFront: Filter[Request, Response, Request, Response] =
      /* Validate host if present to be present in pre-configured list*/
      HostHeaderFilter(config.allowedDomains) andThen
      /* Convert exceptions to responses */
      ExceptionFilter()

    RoutingService.byPath {
      case "/health" =>
        AccessLogFilter(config.accessLogConfig.fileName, config.accessLogConfig.fileSizeInMegaBytes) andThen
          /* Convert exceptions to responses */
          ExceptionFilter() andThen
          HealthCheckService(registry, BpBuild.BuildInfo.version)

      /** Logout */
      case "/logout" =>
        serviceChainFront andThen
          AccessLogFilter(config.accessLogConfig.fileName, config.accessLogConfig.fileSizeInMegaBytes) andThen
          CustomerIdFilter(serviceMatcher) andThen /* Validate that its our service */
          LogoutService(config.sessionStore)

      case _ =>
        serviceChainFront andThen
          /* Generate the Access Log */
          AccessLogFilter(config.accessLogConfig.fileName, config.accessLogConfig.fileSizeInMegaBytes) andThen
          /* Validate that its our service */
          CustomerIdFilter(serviceMatcher) andThen
          /* Get or allocate Session/SignedId */
          SessionIdFilter(serviceMatcher, config.sessionStore) andThen
          /* If unauthenticated, send it to Identity Provider or login page */
          SendToIdentityProvider(identityProviderChainMap(config.sessionStore), config.sessionStore) andThen
          /* If authenticated and protected service, send it via Access Issuer chain */
          SendToAccessIssuer(accessIssuerChainMap(config.sessionStore)) andThen
          /* Authenticated or not, send it to unprotected service, if its destined to that */
          SendToUnprotectedService(config.sessionStore) andThen
          /* Not found */
          notFoundService
    }
  }

  //  Mock Tokenmaster identityEndpoint
  val mockTokenmasterIdentityService = new Service[Request, Response] {
    val tokens = Tokens(MasterToken("masterT"), ServiceTokens())
    val userMap: Map[String, String] = Map(
      ("test1@example.com" -> "password1")
    )

    case class IdentityProviderPod(email: String, password: String)
    implicit val idpPodDecoder: Decoder[IdentityProviderPod] = Decoder.instance {c =>
      for {
        email <- c.downField("email").as[String]
        password <- c.downField("password").as[String]
      } yield IdentityProviderPod(email, password)
    }

    def decode(req: Request): Option[IdentityProviderPod] = {
      req.contentType match {
        case Some(c) if c.contains("application/json") =>
          jawn.decode[IdentityProviderPod](req.contentString).toOption
        case _ => (req.params.get("email"), req.params.get("password")) match {
          case (Some(e), Some(p)) => Some(IdentityProviderPod(e, p))
          case _ => None
        }
      }
    }

    def apply(request: Request): Future[Response] = {
      decode(request).fold(Response(Status.Unauthorized))(_ =>
        tap(Response(Status.Ok))(res => {
          res.contentString = TokensEncoder(tokens).toString()
          res.contentType = "application/json"
        })
      ).toFuture
    }
  }

  //  Mock Tokenmaster AccessIssuer
  val mockTokenmasterAccessIssuerService = new Service[Request, Response] {
    case class AccessIssuerPod(s: String)
    implicit val accessIssuerPodDecoder: Decoder[AccessIssuerPod] = Decoder.instance { c =>
      c.downField("services").as[String].map(AccessIssuerPod(_)) }

    def decode(req: Request): Option[AccessIssuerPod] = {
      req.contentType match {
        case Some(c) if c.contains("application/json") =>
          jawn.decode[AccessIssuerPod](req.contentString).toOption
        case _ => req.params.get("services").map(AccessIssuerPod(_))
      }
    }

    def apply(request: Request): Future[Response] = {
      decode(request).fold(Response(Status.Unauthorized)) { pod =>
        val tokens = Tokens(MasterToken("masterT"), ServiceTokens().add(
          pod.s, ServiceToken(s"SomeServiceData:${pod.s}")))
        tap(Response(Status.Ok))(res => {
          res.contentString = TokensEncoder(tokens).toString()
          res.contentType = "application/json"
        })
      }.toFuture
    }
  }

  //  Mock Login Service
  def mockLoginService(loginConfirm: Path): Service[Request, Response] = new Service[Request, Response] {
    val loginForm = Buf.Utf8(
      s"""<html><body>
        |<h1>Example Account Service Login</h1>
        |<form action=$loginConfirm method="post">
        |<label>username</label><input type="text" name="username" />
        |<label>password</label><input type="password" name="password" />
        |<input type="submit" name="login" value="login" />
        |</form>
        |</body></html>
      """.stripMargin
    )

    def apply(req: Request): Future[Response] =
      req.method match {
        case Method.Get =>
          tap(Response(Status.Ok)) { resp =>
            resp.contentType = "text/html"
            resp.content = loginForm
        }.toFuture
        case _ => Future.value(Response(Status.NotFound))
      }
  }

  //  Mock Upstream service
  val mockUpstreamService = new Service[Request, Response] {
    def apply(request: Request): Future[Response] =
      tap(Response(Status.Ok))(res => {
        res.contentString =
          s"""
             |<html><body>
             |<h1>Welcome to Service @(${request.path})</h1>
                                                        |</body></html>
          """.stripMargin
        res.contentType = "text/html"
      }).toFuture
  }

  //  Mock Aad authenticate service
  def mockAadAuthenticateService(redirectUrl: URL): Service[Request, Response] = new Service[Request, Response] {
    def apply(request: Request): Future[Response] =
      tap(Response(Status.Found))(res => {
        res.location = redirectUrl.toString + Request.queryString("code" -> "XXYYZZ")
      }).toFuture
  }

  //  Mock Aad token service
  val mockAadTokenService = new Service[Request, Response] {
    def apply(request: Request): Future[Response] =
      tap(Response(Status.Ok))(res => {
        res.contentString = """access token""" //***FIXME
        res.setContentTypeJson()
      }).toFuture
  }

  //  Mock Aad certificate service
  val mockAadCertificateService = new Service[Request, Response] {
    def apply(request: Request): Future[Response] =
      tap(Response(Status.Ok))(res => {
        res.contentString = """certificate""" //***FIXME
        res.setContentTypeJson()
      }).toFuture
  }

  // Mock Routing service
  def getMockRoutingService(implicit config: ServerConfig, statsReceiver: StatsReceiver,
                            secretStore: SecretStoreApi): Service[Request, Response] = {
    Try {
      val internalLm = config.findLoginManager("internal").asInstanceOf[BasicLoginManager]
      val externalLm = config.findLoginManager("external").asInstanceOf[OAuth2LoginManager]
      val externalPostPath = Path("aadPostPath")
      val tokenmasterIdEndpoint = config.findEndpoint("tokenmaster-identity-example")
      val tokenmasterAccessEndpoint = config.findEndpoint("tokenmaster-access-example")
      val serviceMatcher = ServiceMatcher(config.customerIdentifiers, config.serviceIdentifiers)

      RoutingService.byPathObject {
        /** Cloud's own identity & access service */
        case tokenmasterAccessEndpoint.path => mockTokenmasterAccessIssuerService
        case tokenmasterIdEndpoint.path => mockTokenmasterIdentityService

        /** Mocking internal authentication */
        case path if path.startsWith(internalLm.authorizePath) => mockLoginService(internalLm.loginConfirm)

        /** *FIXME: Mocking external AAD authentication */
        //case path if path.startsWith(externalLm.authorizeEndpoint.path) => mockLoginService(externalPostPath)
        //case path if path.equals(externalPostPath) =>
        //  mockAadAuthenticateService(new URL(s"http://api.localhost:8080${externalLm.loginConfirm}"))
        //case path if path.startsWith(externalLm.tokenEndpoint.path) => mockAadTokenService
        //case path if path.startsWith(externalLm.certificateEndpoint.path) => mockAadCertificateService

        /** Upstream */
        case _ => mockUpstreamService
      }
    } match {
      case Success(a) => a
      case Failure(e) =>
        // Workaround, when testing with other configurations
        Service.mk[Request, Response] { _ => Response(Status.Ok).toFuture }
    }
  }
}
