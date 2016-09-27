package com.lookout.borderpatrol.sessionx

import com.lookout.borderpatrol.{HealthCheck, HealthCheckStatus}
import com.twitter.finagle.http.Status
import com.twitter.io.Buf
import com.twitter.util.{Future, Time}
import com.twitter.finagle.memcached
import com.twitter.logging.Logger

import scala.collection.mutable
import scala.util.{Failure, Success}


/**
 * Session store that will store a `Session[_]` data into
 */
trait SessionStore {
  def update[A](session: Session[A])(implicit ev: SessionDataEncoder[A]): Future[Unit]
  def get[A](key: SignedId)(implicit ev: SessionDataEncoder[A]): Future[Option[Session[A]]]
  def delete(key: SignedId): Future[Unit]
}

/**
 * Default implementations of [[com.lookout.borderpatrol.sessionx.SessionStore SessionStore]] with
 * [[com.twitter.finagle.memcached memcached]] and an in-memory store for mocking
 */
object SessionStores {
  private[this] val log = Logger.get(getClass.getPackage.getName)

  /**
   * Memcached backend to [[com.lookout.borderpatrol.sessionx.SessionStore SessionStore]]
   *
   * {{{
   *   val store = MemcachedStore(Memcached.newKetamaClient("localhost:11211"))
   *   val requestSession = store.get[http.Request](id) // default views from `Buf` %> `Request` are included
   *   requestSession.onSuccess(s => log(s"Success! you were going to ${s.data.uri}"))
   *                 .onFailure(log)
   * }}}
   *
   * @param store finagle [[com.twitter.finagle.memcached.BaseClient memcached.BaseClient]] memcached backend
   */
  case class MemcachedStore(store: memcached.BaseClient[Buf])
      extends SessionStore {
    val flag = 0 // ignored flag required by memcached api

    /**
     * Fetches a [[com.lookout.borderpatrol.sessionx.Session Session]] if one exists otherwise `None`. On failure
     * will make a [[com.twitter.util.Future.exception Future.exception]].
     *
     * @param key lookup key
     * @param ev evidence for converting the Buf to the type of A
     * @tparam A [[Session.data]] type that must have a view from `Buf %> Option[A]`
     * @return
     */
    def get[A](key: SignedId)(implicit ev: SessionDataEncoder[A]): Future[Option[Session[A]]] =
      store.get(key.asBase64).flatMap {
        case None =>
          log.info(s"Failed to find the Session data for SessionId: ${key.toLogIdString}")
          None.toFuture
        case Some(buf) =>
          ev.decode(buf) match {
            case Failure(e) =>
              log.info(s"Failed to decode the Session data for SessionId: ${key.toLogIdString}, error: ${e.toString}")
              None.toFuture
            case Success(data) =>
              Some(Session(key, data)).toFuture
          }
      }

    /**
     * Stores a [[com.lookout.borderpatrol.sessionx.Session Session]]. On failure returns a
     * [[com.twitter.util.Future.exception Future.exception]]
     *
     * @param session
     * @param ev evidence for the conversion to `Buf`
     * @tparam A [[Session.data]] type that must have a view from `A %> Buf`
     * @return a [[com.twitter.util.Future Future]]
     */
    def update[A](session: Session[A])(implicit ev: SessionDataEncoder[A]): Future[Unit] =
      store.set(session.id.asBase64, flag, session.id.expires, ev.encode(session.data))

    def delete(key: SignedId): Future[Unit] =
      store.delete(key.id.asBase64).map(_ => ())
  }

  /**
   * An in-memory store for prototyping and testing remote stores
   */
  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.MutableDataStructures"))
  case object InMemoryStore extends SessionStore {

    val store: mutable.Set[Session[Buf]] = mutable.Set[Session[Buf]]()

    def get[A](key: SignedId)(implicit ev: SessionDataEncoder[A]): Future[Option[Session[A]]] =
      store.find(_.id == key) match {
        case Some(s) => ev.decode(s.data) match {
          case Failure(e) => None.toFuture
          case Success(data) => Some(Session(key, data)).toFuture
        }
        case _ => None.toFuture
      }

    def update[A](session: Session[A])(implicit ev: SessionDataEncoder[A]): Future[Unit] = {
      delete(session.id)
      if (store.add(session.map(ev.encode))) Future.Unit
      else Future.exception[Unit](new BpSessionStoreError(s"store update failed for ${session.id}"))
    }

    def delete(key: SignedId): Future[Unit] =
      store.find(_.id == key) match {
        case Some(s) => Future.value(store.remove(s)).map(_ => ())
        case None => Future.value(())
      }
  }

  case class MemcachedHealthCheck(name:String, store: MemcachedStore)(implicit secretStore: SecretStoreApi)
      extends HealthCheck {
    /** Create a transient Session, verify the Memcached health with a read and write */
    def check(): Future[HealthCheckStatus] = {
      for {
        data <- Time.now.hashCode().toFuture
        sessionId <- SignedId.transient
        session <- Session(sessionId, data).toFuture
        _ <- store.update(session)
        sessionMaybe <- store.get[Int](sessionId)
      } yield sessionMaybe match {
          case None => HealthCheckStatus.unhealthy(Status.InternalServerError, "get operation failed")
          case Some(s) =>
            if (s.data == data) HealthCheckStatus.healthy
            else HealthCheckStatus.unhealthy(Status.InternalServerError, "set operation failed")
        }
    }
  }
  /*
  implicit object SessionEncryptor extends Encryptable[PSession, SignedId, Array[Byte]] {
    def apply(a: PSession)(key: SignedId): Array[Byte] =
      CryptKey(key).encrypt[PSession](a)
  }

  implicit object SessionDecryptor extends Decryptable[Array[Byte], SignedId, PSession] {
    def apply(e: Array[Byte])(key: SignedId): PSession =
      CryptKey(key).decryptAs[PSession](e)
  }

  implicit object SessionCryptographer extends SessionCrypto[Array[Byte]]

  case class EncryptedInMemorySessionStore(
        store: mutable.Map[String, Encrypted] = mutable.Map[String, Encrypted]())(
      implicit i2s: SignedId %> String)
      extends EncryptedSessionStore[Encrypted, mutable.Map[String, Encrypted]] {

    def update(key: SignedId)(value: PSession) =
      store.put(i2s(key), value.encrypt).
          fold(Future.exception[Unit](new
              UpdateStoreException(s"Unable to update store with $value")))(_ => Future.value[Unit](()))

    def get(key: SignedId) =
      store.get(i2s(key)).flatMap(decrypt(_)).filterNot(session => session.id.expired).toFuture
  }
  */
}

