package com.lookout.borderpatrol.test.sessionx

import com.lookout.borderpatrol.sessionx._
import com.lookout.borderpatrol.test._
import com.twitter.finagle.http.Status
import com.twitter.finagle.memcached.GetResult
import com.twitter.io.Buf
import com.twitter.util.{Time, Future, Await}
import com.twitter.finagle.http
import com.twitter.finagle.memcached


class SessionStoreSpec extends BorderPatrolSuite {
  import coreTestHelpers._
  import SessionStores._

  behavior of "SessionStore"

  val sessionStore = SessionStores.InMemoryStore
  val memcachedSessionStore = SessionStores.MemcachedStore(new memcached.MockClient())
  val intSession = sessions.create(1)
  val strSession = sessions.create("hello")
  val reqSession = sessions.create(http.Request("localhost:8080/api/hello"))

  val stores: List[SessionStore] = List(sessionStore, memcachedSessionStore)

  stores.map { store =>
    /* setup */
    Await.all(
      store.update[Int](intSession),
      store.update[String](strSession),
      store.update[http.Request](reqSession)
    )

    it should s"fetch sessions that are stored in $store" in {
      store.get[String](strSession.id).results.value.data shouldEqual strSession.data
      store.get[Int](intSession.id).results.value.data shouldBe intSession.data
    }

    it should s"return a None when not present in $store" in {
      store.get[Int](sessionid.untagged).results shouldBe None
    }

    it should s"store request sessions $store" in {
      store.get[http.Request](reqSession.id).results.get.data.uri shouldEqual reqSession.data.uri
    }

    it should s"return a None when decoding to wrong type in $store" in {
      store.get[http.Request](intSession.id).results should be(None)
    }

    it should s"delete stored values in $store" in {
      store.update(intSession)
      store.get[Int](intSession.id).results shouldBe Some(intSession)
      store.delete(intSession.id)
      store.get[Int](intSession.id).results shouldBe None
    }
  }

  behavior of "MemcachedHealthCheck"

  it should "Successfully check health of Memcached store" in {
    val memcachedCheck = MemcachedHealthCheck("memcached", memcachedSessionStore)
    Await.result(memcachedCheck.execute()).status should be (Status.Ok)
  }

  it should "return failure when set operation throws an exception" in {
    //  Mock SessionStore client
    case object FailingUpdateMockClient extends memcached.MockClient {
      override def set(key: String, flags: Int, expiry: Time, value: Buf) : Future[Unit] = {
        Future.exception[Unit](new Exception("whoopsie"))
      }
    }
    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingUpdateMockClient)
    val memcachedCheck = MemcachedHealthCheck("memcached", mockSessionStore)

    // Execute
    val output = memcachedCheck.execute()

    //  Verify
    Await.result(output).status should be (Status.InternalServerError)
    Await.result(output).messageStr should be (Some("whoopsie"))
  }

  it should "return failure when get operation throws an exceptions" in {
    //  Mock SessionStore client
    case object FailingGetMockClient extends memcached.MockClient {
      override def getResult(keys: Iterable[String]): Future[GetResult] = {
        Future.exception(new Exception("oopsie"))
      }
    }
    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingGetMockClient)
    val memcachedCheck = MemcachedHealthCheck("memcached", mockSessionStore)

    // Execute
    val output = memcachedCheck.execute()

    //  Verify
    Await.result(output).status should be (Status.InternalServerError)
    Await.result(output).messageStr should be (Some("oopsie"))
  }

  it should "return failure when get operation fails" in {
    //  Mock SessionStore client
    case object FailingGetMockClient extends memcached.MockClient {
      override def getResult(keys: Iterable[String]): Future[GetResult] = {
        delete(keys.head)
        super.getResult(keys)
      }
    }
    // Mock sessionStore
    val mockSessionStore = MemcachedStore(FailingGetMockClient)
    val memcachedCheck = MemcachedHealthCheck("memcached", mockSessionStore)

    // Execute
    val output = memcachedCheck.execute()

    //  Verify
    Await.result(output).status should be (Status.InternalServerError)
    Await.result(output).messageStr should be (Some("get operation failed"))
  }

}
