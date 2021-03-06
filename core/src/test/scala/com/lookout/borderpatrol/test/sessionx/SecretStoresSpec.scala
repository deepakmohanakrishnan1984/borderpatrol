package com.lookout.borderpatrol.test.sessionx

import java.net.URL
import javax.xml.bind.DatatypeConverter

import com.lookout.borderpatrol.Endpoint
import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.sessionx.SecretStores._
import com.lookout.borderpatrol.sessionx.ConsulResponse._
import com.lookout.borderpatrol.test._
import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.finagle.Service
import com.twitter.finagle.http.service.RoutingService
import com.twitter.finagle.http.{Method, Status, Response, Request}
import com.twitter.util.Await
import io.circe.generic.auto._
import io.circe.syntax._


class SecretStoresSpec extends BorderPatrolSuite {
  import coreTestHelpers.{secretStore => store, _}, secrets._

  override def afterEach(): Unit = {
    try {
      super.afterEach() // To be stackable, must call super.afterEach
    }
    finally {
      Endpoint.clearCache()
    }
  }

  val consulResponse = ConsulResponse(5, 0x11, "SomeKey", 5, 5, testSecrets)
  val expiredConsulResponse = ConsulResponse(0, 0, "failed", 0, 0, testExpiredSecrets)
  val consulUrls = Set(new URL("http://localhost:6789"))
  val consulKey = "TestBpKey"

  behavior of "SecretStoreApi"

  it should "give the current and previous Secret" in {
    store.current should be(current)
    store.previous should be(previous)
  }

  it should "always give a non-expired current secret" in {
    // set the current to an expired secret
    val tempStore = SecretStores.InMemorySecretStore(Secrets(previous, previous))
    tempStore.current should not be previous
    tempStore.current.expired should be(false)
  }

  it should "find secrets if they exist" in {
    store.find(_.id == previous.id).value should be(previous)
    store.find(_.id == current.id).value should be(current)
    store.find(_.id == invalid.id) should be(None)
  }

  behavior of "ConsulResponse"

  it should "uphold encoding/decoding secrets" in {
    consulResponse.secretsForValue() should be (testSecrets)
  }

  it should "uphold encoding/decoding ConsulResponse" in {
    def encodeDecode(cr: ConsulResponse): ConsulResponse = {
      val encoded = ConsulResponseEncoder(cr)
      ConsulResponseDecoder.decodeJson(encoded).fold[ConsulResponse](e => expiredConsulResponse, t => t)
    }
    encodeDecode(consulResponse) should be (consulResponse)
  }

  it should "throw an exception if it fails to decode 64bit encoded json string" in {
    val cr1 = ConsulResponse(5, 0x11, "SomeKye", 5, 5, "GARBAGE")
    val caught = the[Exception] thrownBy {
      cr1.secretsForValue()
    }
    caught.getMessage should include ("Failed to decode ConsulResponse from the JSON string with: ")
  }

  it should "throw an exception if it fails to decode ConsulResponse from json string" in {
    val cr1 = ConsulResponse(5, 0x11, "SomeKye", 5, 5,
      DatatypeConverter.printBase64Binary("""{"key":"value"}""".getBytes))
    val caught = the[Exception] thrownBy {
      cr1.secretsForValue()
    }
    caught.getMessage should include ("Failed to decode ConsulResponse from the JSON string with: ")
  }

  behavior of "ConsulSecretStore"

  it should "succeed to find valid secrets in the Step-1 GET and skip the following Step(s)" in {
    var serverModifyIndex = 0
    val server = com.twitter.finagle.Http.serve(
      "localhost:6789",
      RoutingService.byMethodAndPath {
        case (Method.Get, _) => Service.mk[Request, Response]{ req =>
          if (serverModifyIndex == 0) {
            // Step-1 GET
            serverModifyIndex = 1
            tap(Response(Status.Ok))(res => {
              res.contentString = Set(consulResponse).asJson.toString
              res.contentType = "application/json"
            }).toFuture
          } else {
            // Step-3 GET
            fail("Step-3 GET should be skipped")
          }
        }
        case (Method.Put, _) => Service.mk[Request, Response] { req =>
          // Step-2 PUT
          fail("Step-2 PUT should be skipped")
        }
      }
    )
    try {
      /* Create secret store */
      val consulSecretStore = ConsulSecretStore(consulKey, consulUrls, testExpiredSecrets)

      /* Lets have pollSecrets completed before the validation */
      Thread.sleep(1000)

      /* Validate */
      consulSecretStore.current should be (testSecrets.current)
      consulSecretStore.previous should be (testSecrets.previous)
      consulSecretStore.current should not be (testExpiredSecrets.current)
      consulSecretStore.timer.stop()

    } finally {
      server.close()
    }
  }

  it should "succeed to set new secrets in Step-2 PUT, if it find expired secrets in the Step-1 GET" in {
    var serverModifyIndex = 0
    val server = com.twitter.finagle.Http.serve(
      "localhost:6789",
      RoutingService.byMethodAndPath {
        case (Method.Get, _) => Service.mk[Request, Response]{ req =>
          if (serverModifyIndex == 0) {
            // Step-1 GET
            serverModifyIndex = 1
            tap(Response(Status.Ok))(res => {
              res.contentString = Set(expiredConsulResponse).asJson.toString
              res.contentType = "application/json"
            }).toFuture
          }
          else {
            // Step-3 GET
            fail("Step-3 GET should be skipped")
          }
        }
        case (Method.Put, _) => Service.mk[Request, Response] { req =>
          // Step-2 PUT
          tap(Response(Status.Ok))(res => {
            res.contentString = "true"
            res.contentType = "text/plain"
          }).toFuture
        }
      }
    )
    try {
      /* Create secret store (we dont care about init secret in this test) */
      val consulSecretStore = ConsulSecretStore(consulKey, consulUrls)

      /* Lets have pollSecrets completed before the validation */
      Thread.sleep(1000)

      /* Validate (previous of new Secrets matches with current of old Secrets) */
      consulSecretStore.previous should be (testExpiredSecrets.current)
      consulSecretStore.timer.stop()

    } finally {
      server.close()
    }
  }

  it should "succeed to set new secrets in Step-2 PUT, if Step-1 GET returns NotFound" in {
    var serverModifyIndex = 0
    val server = com.twitter.finagle.Http.serve(
      "localhost:6789",
      RoutingService.byMethodAndPath {
        case (Method.Get, _) => Service.mk[Request, Response]{ req =>
          if (serverModifyIndex == 0) {
            // Step-1 GET
            serverModifyIndex = 1
            Response(Status.NotFound).toFuture
          }
          else {
            // Step-3 GET
            fail("Step-3 GET should be skipped")
          }
        }
        case (Method.Put, _) => Service.mk[Request, Response] { req =>
          // Step-2 PUT
          tap(Response(Status.Ok))(res => {
            res.contentString = "true"
            res.contentType = "text/plain"
          }).toFuture
        }
      }
    )
    try {
      /* Create secret store (we dont care about init secret in this test) */
      val consulSecretStore = ConsulSecretStore(consulKey, consulUrls, testSecrets)

      /* Lets have pollSecrets completed before the validation */
      Thread.sleep(1000)

      /* Validate (previous of new Secrets matches with current of old Secrets) */
      consulSecretStore.previous should be (testSecrets.current)
      consulSecretStore.timer.stop()

    } finally {
      server.close()
    }
  }

  it should "succeed to get new secrets in Step-3 GET, if Step-1 GET returns NotFound, Step-2 PUT returns false" in {
    var serverModifyIndex = 0
    val server = com.twitter.finagle.Http.serve(
      "localhost:6789",
      RoutingService.byMethodAndPath {
        case (Method.Get, _) => Service.mk[Request, Response]{ req =>
          if (serverModifyIndex == 0) {
            // Step-1 GET
            serverModifyIndex = 1
            Response(Status.NotFound).toFuture
          }
          else {
            // Step-3 GET
            tap(Response(Status.Ok))(res => {
              res.contentString = Set(consulResponse).asJson.toString
              res.contentType = "application/json"
            }).toFuture
          }
        }
        case (Method.Put, _) => Service.mk[Request, Response] { req =>
          // Step-2 PUT
          tap(Response(Status.Ok))(res => {
            res.contentString = "false"
            res.contentType = "text/plain"
          }).toFuture
        }
      }
    )
    try {
      /* Create secret store */
      val consulSecretStore = ConsulSecretStore(consulKey, consulUrls, testExpiredSecrets)

      /* Lets have pollSecrets completed before the validation */
      Thread.sleep(1000)

      /* Validate */
      consulSecretStore.current should be (testSecrets.current)
      consulSecretStore.previous should be (testSecrets.previous)
      consulSecretStore.timer.stop()

    } finally {
      server.close()
    }
  }

  it should "catch exception thrown while parsing Step-1 GET response, use extant secrets until next attempt" in {
    var serverModifyIndex = 0
    val cr1 = ConsulResponse(5, 0x11, "SomeKye", 5, 5,
      DatatypeConverter.printBase64Binary("""{"key":"value"}""".getBytes))
    val server = com.twitter.finagle.Http.serve(
      "localhost:6789",
      RoutingService.byMethodAndPath {
        case (Method.Get, _) => Service.mk[Request, Response]{ req =>
          if (serverModifyIndex == 0) {
            // Step-1 GET
            serverModifyIndex = 1
            tap(Response(Status.Ok))(res => {
              res.contentString = Set(cr1).asJson.toString
              res.contentType = "application/json"
            }).toFuture
          }
          else {
            // Step-3 GET
            fail("Step-3 GET should be skipped")
          }
        }
        case (Method.Put, _) => Service.mk[Request, Response] { req =>
          // Step-2 PUT
          fail("Step-2 PUT should be skipped")
        }
      }
    )
    try {
      /* Create secret store */
      val consulSecretStore = ConsulSecretStore(consulKey, consulUrls, testSecrets)

      /* Lets have pollSecrets completed before the validation */
      Thread.sleep(1000)

      /* Validate (exception thrown by Step-1 GET parsing is caught internally, _secrets remains unchanged) */
      consulSecretStore.current should be (testSecrets.current)
      consulSecretStore.previous should be (testSecrets.previous)
      consulSecretStore.timer.stop()

    } finally {
      server.close()
    }
  }

  it should "catch exception thrown by Step-1 GET response error status, use extant secrets until next attempt" in {
    var serverModifyIndex = 0
    val cr1 = ConsulResponse(5, 0x11, "SomeKye", 5, 5,
      DatatypeConverter.printBase64Binary("""{"key":"value"}""".getBytes))
    val server = com.twitter.finagle.Http.serve(
      "localhost:6789",
      RoutingService.byMethodAndPath {
        case (Method.Get, _) => Service.mk[Request, Response]{ req =>
          if (serverModifyIndex == 0) {
            // Step-1 GET
            serverModifyIndex = 1
            Response(Status.NotAcceptable).toFuture
          }
          else {
            // Step-3 GET
            fail("Step-3 GET should be skipped")
          }
        }
        case (Method.Put, _) => Service.mk[Request, Response] { req =>
          // Step-2 PUT
          fail("Step-2 PUT should be skipped")
        }
      }
    )
    try {
      /* Create secret store */
      val consulSecretStore = ConsulSecretStore(consulKey, consulUrls, testSecrets)

      /* Lets have pollSecrets completed before the validation */
      Thread.sleep(1000)

      /*
       * Validate
       * (exception thrown by Step-1 GET response error status is caught internally, _secrets remains unchanged)
       */
      consulSecretStore.current should be (testSecrets.current)
      consulSecretStore.previous should be (testSecrets.previous)
      consulSecretStore.timer.stop()

    } finally {
      server.close()
    }
  }

  it should "catch exception thrown by Step-2 PUT response error status, use extant secrets until next attempt" in {
    var serverModifyIndex = 0
    val server = com.twitter.finagle.Http.serve(
      "localhost:6789",
      RoutingService.byMethodAndPath {
        case (Method.Get, _) => Service.mk[Request, Response]{ req =>
          if (serverModifyIndex == 0) {
            // Step-1 GET
            serverModifyIndex = 1
            Response(Status.NotFound).toFuture
          }
          else {
            // Step-3 GET
            fail("Step-3 GET should be skipped")
          }
        }
        case (Method.Put, _) => Service.mk[Request, Response] { req =>
          // Step-2 PUT
          Response(Status.NotFound).toFuture
        }
      }
    )
    try {
      /* Create secret store */
      val consulSecretStore = ConsulSecretStore(consulKey, consulUrls, testSecrets)

      /* Lets have pollSecrets completed before the validation */
      Thread.sleep(1000)

      /* Validate (exception thrown is caught internally, _secrets remains unchanged) */
      consulSecretStore.current should be (testSecrets.current)
      consulSecretStore.previous should be (testSecrets.previous)
      consulSecretStore.timer.stop()

    } finally {
      server.close()
    }
  }

  it should "log a message if Step-3 GET returns expired Secrets, use extant secrets until next attempt" in {
    var serverModifyIndex = 0
    val server = com.twitter.finagle.Http.serve(
      "localhost:6789",
      RoutingService.byMethodAndPath {
        case (Method.Get, _) => Service.mk[Request, Response]{ req =>
          if (serverModifyIndex == 0) {
            // Step-1 GET
            serverModifyIndex = 1
            Response(Status.NotFound).toFuture
          }
          else {
            // Step-3 GET
            tap(Response(Status.Ok))(res => {
              res.contentString = Set(expiredConsulResponse).asJson.toString
              res.contentType = "application/json"
            }).toFuture
          }
        }
        case (Method.Put, _) => Service.mk[Request, Response] { req =>
          // Step-2 PUT
          tap(Response(Status.Ok))(res => {
            res.contentString = "false"
            res.contentType = "text/plain"
          }).toFuture
        }
      }
    )
    try {
      /* Create secret store */
      val consulSecretStore = ConsulSecretStore(consulKey, consulUrls, testSecrets)

      /* Lets have pollSecrets completed before the validation */
      Thread.sleep(1000)

      /* Validate (failure is handled internally, Secrets remain unchanged due to failure) */
      consulSecretStore.current should be (testSecrets.current)
      consulSecretStore.previous should be (testSecrets.previous)
      consulSecretStore.timer.stop()

    } finally {
      server.close()
    }
  }

  it should "catch exception thrown while parsing Step-3 GET response, use extant secrets until next attempt" in {
    var serverModifyIndex = 0
    val cr1 = ConsulResponse(5, 0x11, "SomeKye", 5, 5,
      DatatypeConverter.printBase64Binary("""{"key":"value"}""".getBytes))
    val server = com.twitter.finagle.Http.serve(
      "localhost:6789",
      RoutingService.byMethodAndPath {
        case (Method.Get, _) => Service.mk[Request, Response]{ req =>
          if (serverModifyIndex == 0) {
            // Step-1 GET
            serverModifyIndex = 1
            Response(Status.NotFound).toFuture
          }
          else {
            // Step-3 GET
            tap(Response(Status.Ok))(res => {
              res.contentString = Set(cr1).asJson.toString
              res.contentType = "application/json"
            }).toFuture
          }
        }
        case (Method.Put, _) => Service.mk[Request, Response] { req =>
          // Step-2 PUT
          tap(Response(Status.Ok))(res => {
            res.contentString = "false"
            res.contentType = "text/plain"
          }).toFuture
        }
      }
    )
    try {
      /* Create secret store */
      val consulSecretStore = ConsulSecretStore(consulKey, consulUrls, testSecrets)

      /* Lets have pollSecrets completed before the validation */
      Thread.sleep(1000)

      /* Validate (exception thrown is caught internally, _secrets remains unchanged) */
      consulSecretStore.current should be (testSecrets.current)
      consulSecretStore.previous should be (testSecrets.previous)
      consulSecretStore.timer.stop()

    } finally {
      server.close()
    }
  }

  behavior of "ConsulHealthCheck"

  it should "succeed it finds a valid secret" in {
    val server = com.twitter.finagle.Http.serve(
      "localhost:6789",
      RoutingService.byMethodAndPath {
        case (Method.Get, _) => Service.mk[Request, Response]{ req =>
          tap(Response(Status.Ok))(res => {
            res.contentString = Set(consulResponse).asJson.toString
            res.contentType = "application/json"
          }).toFuture
        }
        case (Method.Put, _) => Service.mk[Request, Response] { req =>
          // Step-2 PUT
          fail("Step-2 PUT should be skipped")
        }
      }
    )
    try {
      /* Create secret store */
      val consulSecretStore = ConsulSecretStore(consulKey, consulUrls, testExpiredSecrets)

      /* Lets have pollSecrets completed before the validation */
      Thread.sleep(1000)

      /* Validate */
      consulSecretStore.current should be (testSecrets.current)
      consulSecretStore.previous should be (testSecrets.previous)
      consulSecretStore.current should not be (testExpiredSecrets.current)

      val consulCheck = ConsulHealthCheck("consul", consulSecretStore)
      Await.result(consulCheck.execute()).status should be (Status.Ok)

      /* End */
      consulSecretStore.timer.stop()

    } finally {
      server.close()
    }
  }

  it should "return failure when it fails to find a valid secret" in {
    val server = com.twitter.finagle.Http.serve(
      "localhost:6789",
      RoutingService.byMethodAndPath {
        case (Method.Get, _) => Service.mk[Request, Response]{ req =>
          // Step-11 GET
          Response(Status.NotFound).toFuture
        }
        case (Method.Put, _) => Service.mk[Request, Response] { req =>
          // Step-2 PUT
          fail("Step-2 PUT should be skipped")
        }
      }
    )
    try {
      /* Create secret store */
      val consulSecretStore = ConsulSecretStore(consulKey, consulUrls, testExpiredSecrets)

      /* Lets have pollSecrets completed before the validation */
      Thread.sleep(1000)

      /* Validate */
      val consulCheck = ConsulHealthCheck("consul", consulSecretStore)
      Await.result(consulCheck.execute()).status should be (Status.InternalServerError)

      /* End */
      consulSecretStore.timer.stop()

    } finally {
      server.close()
    }
  }

}
