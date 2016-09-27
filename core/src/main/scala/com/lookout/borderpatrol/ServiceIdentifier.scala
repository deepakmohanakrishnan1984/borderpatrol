package com.lookout.borderpatrol

import java.net.URL

import com.twitter.finagle.http.path.Path

/**
 * An identifier for Border Patrol to determine by `path` which service a request
 * should be routed to
 *
 * @param name The name that can be used to refer to a [[com.twitter.finagle.Name]]
 * @param hosts The list of URLs to upstream service
 * @param path The external url path prefix that routes to the internal service
 * @param rewritePath The (optional) internal url path prefix to the internal service. If present,
 *                    it replaces the external path in the Request URI
 * @param unprotectedPaths The list of unprotected/unauthenticated paths (i.e. does not go through access issuer)
 *                         anchored on the "path"
 */
case class ServiceIdentifier(name: String, hosts: Set[URL], path: Path, rewritePath: Option[Path],
                             unprotectedPaths: Set[Path]) {
  def isServicePath(p: Path): Boolean = p.startsWith(path)
  def isUnprotected(s: String): Boolean = unprotectedPaths.exists { p =>
    val components = path.toList ++ p.toList
    (Path(s).toList take components.length) == components
  }
  lazy val endpoint: Endpoint = SimpleEndpoint(name, path, hosts)
}

/**
 * An identifier for Border Patrol to determine by `subdomain` which service a request
 * should be routed to
 *
 * @param subdomain
 * @param defaultServiceId
 * @param loginManager
 */
case class CustomerIdentifier(subdomain: String, guid: String, defaultServiceId: ServiceIdentifier,
                              loginManager: LoginManager)
