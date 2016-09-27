package com.lookout.borderpatrol.server

import java.net.URL

import com.lookout.borderpatrol._
import com.lookout.borderpatrol.sessionx.SecretStores._
import com.lookout.borderpatrol.sessionx.SessionStores._
import com.lookout.borderpatrol.sessionx._
import com.twitter.finagle.Memcached
import com.twitter.finagle.http.path.Path
import cats.data.Xor
import com.twitter.finagle.service.FailureAccrualFactory
import com.twitter.logging.Logger
import com.twitter.util.Duration
import io.circe.{Encoder, _}
import io.circe.syntax._
import com.google.common.net.InternetDomainName
import java.nio.file._
import scala.util.Try
/**
  * AccessLog Filter Configuration
  *
  * @param logDestination with absolute path.
  * @param fileSizeInMegaBytes
  * @param fileCount
  */
case class AccessLogConfig(logDestination: String, fileSizeInMegaBytes: Long, fileCount: Int)
/**
 * Where you will find the Secret Store and Session Store
 */
object Config {
  val defaultSecretStore = SecretStores.InMemorySecretStore(Secrets(Secret(), Secret()))
  val defaultSessionStore = SessionStores.InMemoryStore
  private[this] val log = Logger.get(getClass.getPackage.getName)

  def cond[T](p: => Boolean, v: T): Set[T] = if (p) Set(v) else Set.empty[T]

  // Encoder/Decoder for Path
  implicit val encodePath: Encoder[Path] = Encoder[String].contramap(_.toString)
  implicit val decodePath: Decoder[Path] = Decoder[String].map(Path(_))

  // Encoder/Decoder for URL
  implicit val encodeUrl: Encoder[URL] = Encoder[String].contramap(_.toString)
  implicit val decodeUrl: Decoder[URL] = Decoder[String].map(new URL(_))

  // Encoder/Decoder for SessionStore
  implicit val encodeSessionStore: Encoder[SessionStore] = Encoder.instance {
    case x: InMemoryStore.type => Json.obj(("type", Json.fromString("InMemoryStore")))
    case y: MemcachedStore => Json.obj(("type", Json.fromString("MemcachedStore")),
      ("hosts", Json.fromString("localhost:123")))
  }
  implicit val decodeSessionStore: Decoder[SessionStore] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "InMemoryStore" => Xor.right(defaultSessionStore)
      case "MemcachedStore" =>
        c.downField("hosts").as[String].map(hosts => SessionStores.MemcachedStore(
          Memcached.client
            .configured(FailureAccrualFactory.Param(1, () => Duration.fromSeconds(60)))
            .configured(Memcached.param.EjectFailedHost(true))
            .newRichClient(s"memcached=${hosts}")))
      case other => Xor.left(DecodingFailure(s"Invalid sessionStore: $other", c.history))
    }
  }

  // Encoder/Decoder for SecretStore
  implicit val encodeSecretStore: Encoder[SecretStoreApi] = Encoder.instance {
    case x: InMemorySecretStore => Json.obj(("type", Json.fromString(x.getClass.getSimpleName)))
    case y: ConsulSecretStore => Json.fromFields(Seq(
      ("type", y.getClass.getSimpleName.asJson),
      ("hosts", y.consulUrls.asJson),
      ("key", y.key.asJson)))
  }
  implicit val decodeSecretStore: Decoder[SecretStoreApi] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "InMemorySecretStore" => Xor.right(defaultSecretStore)
      case "ConsulSecretStore" =>
        for {
          hosts <- c.downField("hosts").as[Set[URL]]
          key <- c.downField("key").as[String]
        } yield ConsulSecretStore(key, hosts)
      case other => Xor.left(DecodingFailure(s"Invalid secretStore: $other", c.history))
    }
  }

  // Encoder/Decoder for ServiceIdentifier
  implicit val encodeServiceIdentifier: Encoder[ServiceIdentifier] = Encoder.instance { sid =>
    Json.fromFields(Seq(
      ("name", sid.name.asJson),
      ("hosts", sid.hosts.asJson),
      ("path", sid.path.asJson),
      ("rewritePath", sid.rewritePath.asJson),
      ("unprotectedPaths", sid.unprotectedPaths.asJson)))
  }
  implicit val decodeServiceIdentifier: Decoder[ServiceIdentifier] =
    Decoder.instance { c =>
      for {
        name <- c.downField("name").as[String]
        hosts <- c.downField("hosts").as[Set[URL]]
        path <- c.downField("path").as[Path]
        rewritePathOption <- c.downField("rewritePath").as[Option[Path]]
        unprotectedPaths <- c.downField("unprotectedPaths").as[Option[Set[Path]]]
      } yield ServiceIdentifier(name, hosts, path, rewritePathOption, unprotectedPaths.getOrElse(Set.empty[Path]))
    }

  // Encoder/Decoder for CustomerIdentifier
  implicit val encodeCustomerIdentifier: Encoder[CustomerIdentifier] = Encoder.instance { cid =>
    Json.fromFields(Seq(
      ("subdomain", cid.subdomain.asJson),
      ("guid", cid.guid.asJson),
      ("defaultServiceIdentifier", cid.defaultServiceId.name.asJson),
      ("loginManager", cid.loginManager.name.asJson)))
  }

  def decodeCustomerIdentifier(sids: Map[String, ServiceIdentifier], lms: Map[String, LoginManager]):
    Decoder[CustomerIdentifier] =
    Decoder.instance { c =>
      for {
        subdomain <- c.downField("subdomain").as[String]
        guid <- c.downField("guid").as[String]
        sidName <- c.downField("defaultServiceIdentifier").as[String]
        sid <- Xor.fromOption(sids.get(sidName),
          DecodingFailure(s"ServiceIdentifier '$sidName' not found: ", c.history)
        )
        lmName <- c.downField("loginManager").as[String]
        lm <- Xor.fromOption(lms.get(lmName),
          DecodingFailure(s"LoginManager '$lmName' not found: ", c.history)
        )
      } yield CustomerIdentifier(subdomain, guid, sid, lm)
    }

  // Encoder/Decoder for InternetDomainName
  implicit val encodeInternetDomainName: Encoder[InternetDomainName] = Encoder[String].contramap(_.toString)
  implicit val decodeInternetDomainName: Decoder[InternetDomainName] = Decoder[String].map(InternetDomainName.from(_))

  /**
    * Validate Hosts (i.e. Set of URLs) configuration
    *
    * @param field
    * @param name
    * @param hosts
    * @return set of all the errors encountered during validation
    */
  def validateHostsConfig(field: String, name: String, hosts: Set[URL]): Set[String] = {
    // Make sure urls in Manager have matching protocol
    (cond[String](hosts.map(_.getProtocol()).size != 1,
      s"hosts configuration for ${name} in ${field}: has differing protocols") ++

      // Make sure hosts in Manager have either http or https protocol
      cond(!hosts.map(_.getProtocol()).mkString.matches("http[s]*"),
        s"hosts configuration for ${name} in ${field}: has unsupported protocol") ++

      // Make sure https hosts have a matching hostname
      cond(!hosts.filter(u => u.getProtocol == "https").isEmpty && hosts.map(u => u.getHost()).size != 1,
        s"hosts configuration for ${name} in ${field}: https urls have mismatching hostnames"))
  }

  /**
    * Validate SecretStore configuration
    *
    * @param field
    * @param secretStores
    * @return set of all the errors encountered during validation
    */
  def validateSecretStoreConfig(field: String, secretStores: SecretStoreApi): Set[String] = {
    secretStores match {
      case x: ConsulSecretStore => validateHostsConfig(field, "consulSecretStore", x.consulUrls)
      case _ => Set.empty[String]
    }
  }

  /**
    * Validate Login Manager configurartion
    *
    * @param field
    * @param loginManagers
    * @return set of all the errors encountered during validation
    */
  def validateLoginManagerConfig(field: String, loginManagers: Set[LoginManager]): Set[String] = {
    // Find if loginManagers have duplicate entries
    cond(loginManagers.size > loginManagers.map(lm => lm.name).size,
      s"Duplicate entries for key (name) are found in the field: ${field}")
  }

  /**
    * Validate serviceIdentifier configuration
    *
    * @param field
    * @param sids
    * @return set of all the errors encountered during validation
    */
  def validateServiceIdentifierConfig(field: String, sids: Set[ServiceIdentifier]): Set[String] = {
    // Log an info message if ServiceIdentifiers have duplicate entries for combination of (name, protected)
    if (sids.size > sids.map(sid => sid.name).size)
      log.info("Potential Configuration Error Alert: " +
        "Duplicate entries for key(name) are found in the field: ServiceIdentifiers")

    // Find if ServiceIdentifiers have duplicate entries (same path)
    (cond(sids.size > sids.map(sid => sid.path).size,
      s"Duplicate entries for key (path) are found in the field: ${field}") ++

      // Make sure hosts in Serviceidentifier have http or https protocol
      sids.map(sid => validateHostsConfig(field, sid.name, sid.hosts)).flatten)
  }

  /**
    * Validate customerIdentifier configuration
    *
    * @param field
    * @param cids
    * @return set of all the errors encountered during validation
    */
  def validateCustomerIdentifierConfig(field: String, cids: Set[CustomerIdentifier]): Set[String] = {
    // Find if CustomerIdentifiers have duplicate entries
    cond(cids.size > cids.map(cid => cid.subdomain).size,
      s"Duplicate entries for key (subdomain) are found in the field: ${field}")
  }

  /**
    * Validate accessLog configuration.
    *
    * @param accessLogConfig
    */

  def validateAccessLogConfig(accessLogConfig: Option[AccessLogConfig]): Set[String] = accessLogConfig match {
    case Some(conf) if (conf.logDestination == ("/dev/stdout") || conf.logDestination == ("/dev/stderr")) => Set.empty
    case Some(conf) => {
      val fs: FileSystem = FileSystems.getDefault()
      val configPath = fs.getPath(conf.logDestination)
      val parentPath = configPath.getParent()
      Try(Files.exists(configPath)).toOption match {
        case Some(s) if s => Set.empty
        case _ if Try {
          Files.createDirectory(parentPath)
          Files.createFile(configPath)
        }.isSuccess => Set.empty
        case _ if Try(Files.createFile(configPath)).isSuccess => Set.empty
        case _ => Set(s"Failed to create file: ${configPath}")
      }
    }
    case None => Set.empty
  }
}
