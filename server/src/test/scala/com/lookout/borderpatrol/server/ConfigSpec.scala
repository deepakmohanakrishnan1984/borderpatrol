package com.lookout.borderpatrol.server

import java.net.URL
import com.google.common.net.InternetDomainName
import com.lookout.borderpatrol.auth.tokenmaster._
import com.lookout.borderpatrol.auth.tokenmaster.LoginManagers._
import com.lookout.borderpatrol.sessionx.SecretStores.ConsulSecretStore
import com.lookout.borderpatrol.sessionx.SessionStores.MemcachedStore
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.test._
import com.lookout.borderpatrol._
import com.twitter.finagle.memcached
import com.twitter.finagle.http.path.{Path, Root}
import cats.data.Xor
import io.circe._
import io.circe.jawn._
import io.circe.generic.auto._
import io.circe.syntax._
import scala.reflect.io.File


class ConfigSpec extends BorderPatrolSuite {
  import coreTestHelpers._
  import tokenmasterTestHelpers._
  import Config._

  override def afterEach(): Unit = {
    try {
      super.afterEach() // To be stackable, must call super.afterEach
    }
    finally {
      Endpoint.clearCache()
    }
  }

  // Stores
  val memcachedSessionStore = SessionStores.MemcachedStore(new memcached.MockClient())
  val consulSecretStore = SecretStores.ConsulSecretStore("testBpKey", Set(new URL("http://localhost:1234")))

  // Helpers
  def decodeCids(json: Json, sids: Set[ServiceIdentifier], lms: Set[LoginManager]) : Set[CustomerIdentifier] = {
    Decoder.decodeCanBuildFrom[CustomerIdentifier, Set](decodeCustomerIdentifier(
      sids.map(sid => sid.name -> sid).toMap, lms.map(l => l.name -> l).toMap), implicitly).decodeJson(json) match {
      case Xor.Right(a) => a
      case Xor.Left(b) => throw new Exception(b.getMessage)
    }
  }
  def decodeCid(json: Json, sids: Set[ServiceIdentifier], lms: Set[LoginManager]) : CustomerIdentifier = {
    decodeCustomerIdentifier(sids.map(sid => sid.name -> sid).toMap,
      lms.map(l => l.name -> l).toMap).decodeJson(json) match {
      case Xor.Right(a) => a
      case Xor.Left(b) => throw new Exception(b.getMessage)
    }
  }

  def validateConsulStore(a: ConsulSecretStore, b: ConsulSecretStore): Unit = {
    a.consulUrls should be(b.consulUrls)
    a.key should be(b.key)
  }

  def validateMemcachedStore(a: MemcachedStore, b: MemcachedStore): Unit = {
    a.flag should be(b.flag)
  }

  behavior of "Config"

  it should "uphold encoding/decoding SecretStore" in {
    decodeSecretStore.decodeJson(defaultSecretStore.asInstanceOf[SecretStoreApi].asJson).toOption should
      be(Some(defaultSecretStore.asInstanceOf[SecretStoreApi]))
    val decodedConsulSecretStoreOpt = decodeSecretStore.decodeJson(
      consulSecretStore.asInstanceOf[SecretStoreApi].asJson).toOption
    decodedConsulSecretStoreOpt should not be(None)
    validateConsulStore(decodedConsulSecretStoreOpt.get.asInstanceOf[ConsulSecretStore], consulSecretStore)
    decodeSecretStore.decodeJson(Json.obj(("type", "InMemorySecretStore".asJson))).toOption should
      be(Some(defaultSecretStore.asInstanceOf[SecretStoreApi]))
    decodeSecretStore.decodeJson(Json.obj(("type", "ConsulSecretStore".asJson))).toOption should be(None)
    decodeSecretStore.decodeJson(Json.obj(("type", Json.fromString("woof")))).toOption should be(None)
  }

  it should "uphold encoding/decoding SessionStore" in {
    decodeSessionStore.decodeJson(defaultSessionStore.asInstanceOf[SessionStore].asJson).toOption should
      be(Some(defaultSessionStore.asInstanceOf[SessionStore]))
    val decodedMemcachedStoreOpt =
      decodeSessionStore.decodeJson(memcachedSessionStore.asInstanceOf[SessionStore].asJson).toOption
    decodedMemcachedStoreOpt should not be(None)
    validateMemcachedStore(decodedMemcachedStoreOpt.get.asInstanceOf[MemcachedStore], memcachedSessionStore)
    decodeSessionStore.decodeJson(Json.obj(("type", "InMemoryStore".asJson))).toOption should
      be(Some(defaultSessionStore.asInstanceOf[SessionStore]))
    decodeSessionStore.decodeJson(Json.obj(("type", "MemcachedStore".asJson))).toOption should be(None)
    decodeSessionStore.decodeJson(Json.obj(("type", Json.fromString("woof")))).toOption should be(None)
  }

  it should "uphold encoding/decoding Path" in {
    decode[Path](""""/foo/bar"""").toOption.get.toString should be("/foo/bar")
    decode[Path](""""/"""").toOption.get.toString should be("")
    decode[Path]("""""""").toOption.get.toString should be("")
    decode[Path]("""" """").toOption.get.toString should be("/ ")
  }

  it should "uphold encoding/decoding ServiceIdentifier" in {
    def decodeFromJson(json: Json): ServiceIdentifier =
      decode[ServiceIdentifier](json.toString()) match {
        case Xor.Right(a) => a
        case Xor.Left(b) => throw new Exception(b.getMessage)
      }

    val partialContents = Json.fromFields(Seq(
      ("name", one.name.asJson),
      ("hosts", one.hosts.asJson),
      ("path", one.path.asJson)))
    decodeFromJson(one.asJson) should be(one)
    decodeFromJson(partialContents) should be(one)
    decodeFromJson(two.asJson) should be(two)
  }

  it should "uphold encoding/decoding CustomerIdentifier" in {
    decodeCid(cust1.asJson, sids, loginManagers) should be (cust1)
    decodeCid(cust2.asJson, sids, loginManagers) should be (cust2)
    decodeCids(cids.asJson, sids, loginManagers) should be (cids)
    decodeCid(cust1k.asJson, sids, loginManagersk) should be (cust1k)
    decodeCid(cust2k.asJson, sids, loginManagersk) should be (cust2k)
    decodeCids(cidsk.asJson, sids, loginManagersk) should be (cidsk)
  }

  it should "raise a BpConfigError exception due to missing LoginManager in CustomerIdentifier config" in {
    val partialContents = Json.array(Json.fromFields(Seq(
        ("subdomain", "some".asJson),
        ("guid", "some".asJson),
        ("defaultServiceIdentifier", "one".asJson),
        ("loginManager", "bad".asJson))))

    val caught = the[Exception] thrownBy {
      decodeCids(partialContents, sids, loginManagersk)
    }
    caught.getMessage should include ("LoginManager 'bad' not found")
  }

  it should "raise a BpConfigError exception due to missing ServiceIdentifier in CustomerIdentifier config" in {
    val partialContents = Json.array(Json.fromFields(Seq(
        ("subdomain", "some".asJson),
        ("guid", "some".asJson),
        ("defaultServiceIdentifier", "bad".asJson),
        ("loginManager", "checkpoint".asJson))))

    val caught = the[Exception] thrownBy {
      decodeCids(partialContents, sids, loginManagersk)
    }
    caught.getMessage should include ("ServiceIdentifier 'bad' not found")
  }

  it should "return a Set with errors if duplicates are configured in loginManagers config" in {
    val output = validateLoginManagerConfig("loginManagers", loginManagersk +
      BasicLoginManager("checkpointLoginManager", "tokenmaster-basic", "some-guid", Path("/some"), None, Path("/some"),
        tokenmasterIdEndpoint, tokenmasterAccessEndpoint))
    output should contain ("Duplicate entries for key (name) are found in the field: loginManagers")
  }

  it should "return a Set with errors if duplicate paths are configured in serviceIdentifiers config" in {
    val output = validateServiceIdentifierConfig("serviceIdentifiers",
      sids + ServiceIdentifier("some", urls, Path("/ent"), None, Set.empty)
        + ServiceIdentifier("some", urls, Path("/some"), None, Set(Root)))
    output should contain ("Duplicate entries for key (path) are found in the field: serviceIdentifiers")
  }

  it should "return a Set with errors if duplicate subdomains are configured in customerIdentifiers config" in {
    val output = validateCustomerIdentifierConfig("customerIdentifiers",
      cids + CustomerIdentifier("enterprise", "some-guid", two, checkpointLoginManager))
    output should contain ("Duplicate entries for key (subdomain) are found in the field: customerIdentifiers")
  }

  it should "validate Hosts configuration" in {
    val u1 = new URL("http://sample.com")
    val u2 = new URL("http://sample.com:8080/goto")
    val u3 = new URL("http://tample.com:2345/foo")
    val u4 = new URL("https://xample.com:2222")
    val u5 = new URL("https://ample.com:2221")
    val u11 = new URL("ftp://localhost:123")

    validateHostsConfig("some", "working1", Set(u1, u2)).isEmpty should be (true)
    validateHostsConfig("some", "working2", Set(u4)).isEmpty should be (true)
    validateHostsConfig("some", "failed1", Set(u3, u4)).mkString should include (
      "hosts configuration for failed1 in some: has differing protocols")
    validateHostsConfig("some", "failed2", Set(u11)).mkString should include (
      "hosts configuration for failed2 in some: has unsupported protocol")
    validateHostsConfig("some", "failed3", Set(u4, u5)).mkString should include (
      "hosts configuration for failed3 in some: https urls have mismatching hostnames")
    validateHostsConfig("some", "failed4", Set()) should contain (
      "hosts configuration for failed4 in some: has unsupported protocol")
  }

  it should "uphold encode decode InternetDomainName to string" in {

    val wiki = InternetDomainName.from("source.corp.wiki.com")
    decodedInternetDomainNameJson(wiki).get should be (wiki)

    val yahoo = InternetDomainName.from("yahoo.com")
    encodeInternetDomainName(yahoo).toString() should be (decodedInternetDomainNameJson(yahoo).asJson.toString())

    a[IllegalArgumentException] should be thrownBy {
      decodeInternetDomainName.decodeJson(500.toString.asJson)
    }

  }

  it should "Succeed to Validate Access Log configuration" in {
    val tempValidateAccessLogFile = File.makeTemp("TempAccessLogFile", ".tmp")
    val tempFileSizeInMegaBytes = 1
    val tempFileCount = 1
    val goodOutputs = Set("/dev/stdout", "/dev/stderr", tempValidateAccessLogFile.toCanonical.toString)

    goodOutputs.foreach { p =>
      val conf = AccessLogConfig(p, tempFileSizeInMegaBytes, tempFileCount)
      validateAccessLogConfig(Some(conf)).isEmpty should be(true)
    }
  }

  it should "Fail to Validate Access Log Configuration" in {
    val tempFileSizeInMegaBytes = 1
    val tempFileCount = 1
    val badOutputs = Set("/dev/foo", "/bad/path/filePath/badFile.txt")
    badOutputs.foreach { p =>
      val conf = AccessLogConfig(p, tempFileSizeInMegaBytes, tempFileCount)
      validateAccessLogConfig(Some(conf)).isEmpty should be(false)
    }
  }

  def decodedInternetDomainNameJson(internetDomainName: InternetDomainName): Option[InternetDomainName] = {
    decodeInternetDomainName.decodeJson(internetDomainName.asJson).toOption
  }
}
