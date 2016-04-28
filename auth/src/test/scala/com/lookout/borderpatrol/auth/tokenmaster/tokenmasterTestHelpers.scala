package com.lookout.borderpatrol.auth.tokenmaster

import java.net.URL

import com.lookout.borderpatrol._
import com.lookout.borderpatrol.auth.tokenmaster.LoginManagers._
import com.twitter.finagle.http.path.Path


object tokenmasterTestHelpers {
  import test.coreTestHelpers._
  import OAuth2._

  // Endpoints
  val ulmAuthorizeEndpoint = Endpoint("ulmAuthorizeEndpoint", Path("/authorize"), Set(new URL("http://example.com")))
  val ulmTokenEndpoint = Endpoint("ulmTokenEndpoint", Path("/token"), Set(new URL("http://localhost:5678")))
  val ulmCertificateEndpoint = Endpoint("ulmCertificateEndpoint", Path("/certificate"),
    Set(new URL("http://localhost:5678")))
  val rlmAuthorizeEndpoint = Endpoint("rlmAuthorizeEndpoint", Path("/authorize"),
    Set(new URL("http://localhost:9999")))
  val rlmTokenEndpoint = Endpoint("rlmTokenEndpoint", Path("/token"), Set(new URL("http://localhost:9999")))
  val rlmCertificateEndpoint = Endpoint("rlmCertificateEndpoint", Path("/certificate"),
    Set(new URL("http://localhost:9999")))
  val endpointsk = Set(tokenmasterIdEndpoint, tokenmasterAccessEndpoint,
    ulmAuthorizeEndpoint, ulmTokenEndpoint, ulmCertificateEndpoint,
    rlmAuthorizeEndpoint, rlmTokenEndpoint, rlmCertificateEndpoint)

  // Login Managers
  val checkpointLoginManager = BasicLoginManager("checkpointLoginManager", "tokenmaster.basic", "cp-guid",
    Path("/loginConfirm"), Path("/check"), tokenmasterIdEndpoint, tokenmasterAccessEndpoint)
  val umbrellaLoginManager = OAuth2LoginManager("ulmLoginManager", "tokenmaster.oauth2", "ulm-guid", Path("/signin"),
    tokenmasterIdEndpoint, tokenmasterAccessEndpoint,
    ulmAuthorizeEndpoint, ulmTokenEndpoint, ulmCertificateEndpoint,
    "clientId", "clientSecret")
  val rainyLoginManager = OAuth2LoginManager("rlmProtoManager", "tokenmaster.oauth2", "rlm-guid", Path("/signblew"),
    tokenmasterIdEndpoint, tokenmasterAccessEndpoint,
    rlmAuthorizeEndpoint, rlmTokenEndpoint, rlmCertificateEndpoint,
    "clientId", "clientSecret")
  val loginManagersk = Set(checkpointLoginManager.asInstanceOf[LoginManager],
    umbrellaLoginManager.asInstanceOf[LoginManager],
    rainyLoginManager.asInstanceOf[LoginManager])

  //  oAuth2 Code Verify object
  val oAuth2CodeVerify = new OAuth2CodeVerify

  // cids
  val cust1k = CustomerIdentifier("enterprise.k", "cust1-guid", one, checkpointLoginManager)
  val cust2k = CustomerIdentifier("sky.k", "cust2-guid", two, umbrellaLoginManager)
  val cust3k = CustomerIdentifier("rainy.k", "cust3-guid", three, rainyLoginManager)
  val cidsk = Set(cust1k, cust2k, cust3k)
}
