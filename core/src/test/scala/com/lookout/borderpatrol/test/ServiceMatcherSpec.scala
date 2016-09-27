package com.lookout.borderpatrol.test

import com.lookout.borderpatrol._
import com.twitter.finagle.http.path.Path


class ServiceMatcherSpec extends BorderPatrolSuite {
  import coreTestHelpers._

  val sOneOne = ServiceIdentifier("eOne", urls, Path("/ent1"), None, Set.empty)
  val sOneTwo = ServiceIdentifier("eTwo", urls, Path("/ent2"), None, Set.empty)
  val sTwo = ServiceIdentifier("two", urls, Path("/api"), None, Set.empty)
  val sThree = ServiceIdentifier("three", urls, Path("/apis"), None, Set.empty)
  val sFour = ServiceIdentifier("four", urls, Path("/apis/test"), None, Set.empty)
  val serviceIds = Set(sOneOne, sOneTwo, sTwo, sThree, sFour)
  val cOne = CustomerIdentifier("enterprise", "c1-guid", sOneOne, test1LoginManager)
  val cTwo = CustomerIdentifier("api", "c2-guid", two, test2LoginManager)
  val cThree = CustomerIdentifier("api.subdomain", "c3-guid", sThree, test1LoginManager)
  val cFour = CustomerIdentifier("api.testdomain", "c4-guid", sFour, test2LoginManager)
  val custIds = Set(cOne, cTwo, cThree, cFour)
  val testServiceMatcher = ServiceMatcher(custIds, serviceIds)

  behavior of "ServiceMatchers"

  it should "match the longest path" in {
    testServiceMatcher.serviceId(Path("/")) should be(None)
    testServiceMatcher.serviceId(Path("/e")) should be(None)
    testServiceMatcher.serviceId(Path("/ent")) should be(None)
    testServiceMatcher.serviceId(Path("/ent1/blah")).value should be(sOneOne)
    testServiceMatcher.serviceId(Path("/ent2")).value should be(sOneTwo)
    testServiceMatcher.serviceId(Path("/api")).value should be(sTwo)
    testServiceMatcher.serviceId(Path("/apis")).value should be(sThree)
    testServiceMatcher.serviceId(Path("/apis/testing")).value should be(sThree)
    testServiceMatcher.serviceId(Path("/apis/test")).value should be(sFour)
  }

  it should "match the longest subdomain" in {
    testServiceMatcher.customerId("www.example.com") should be(None)
    testServiceMatcher.customerId("enterprise.api.example.com").value should be(cOne)
    testServiceMatcher.customerId("enterprise.example.com").value should be(cOne)
    testServiceMatcher.customerId("api.example.com").value should be(cTwo)
    testServiceMatcher.customerId("api.subdomains.example.com").value should be(cTwo)
    testServiceMatcher.customerId("api.subdomain.example.com").value should be(cThree)
  }
}
