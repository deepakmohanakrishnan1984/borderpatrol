package com.lookout.borderpatrol.server

import com.lookout.borderpatrol.util.Combinators.tap
import com.twitter.util._
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.logging.{Logger, _}
import com.twitter.util.Future
import com.twitter.conversions.storage._

/**
  * Filter to generate access logs. The logs generated
  *
  * @param logDestination
  * @param fileSizeInMegaBytes
  * @param fileCount
  */

case class AccessLogFilter(logDestination: String, fileSizeInMegaBytes: Long, fileCount: Int)
  extends SimpleFilter[Request, Response] {

  val accessLogLevel: Level = Level.ALL
  val loggerName = "BorderPatrol_Access_Logs"
  val accessLogHandler = {
    if (logDestination == "/dev/stderr" || logDestination == "/dev/stdout")
      ConsoleHandler(BareFormatter, Some(accessLogLevel))
    else {
      FileHandler(
        filename = logDestination,
        rollPolicy = Policy.MaxSize(fileSizeInMegaBytes.megabytes),
        level = Some(accessLogLevel),
        rotateCount = fileCount,
        append = true,
        formatter = BareFormatter
      )
    }
  }.apply()

  val logger: Logger = {
    tap(Logger.get(loggerName)) { l =>
      l.clearHandlers()
      // This allows the AccessLog logger to be used as a separate logger, not under root logger.
      l.setUseParentHandlers(false)
      //Set Log level for this logger.
      l.setLevel(accessLogLevel)
      // Add handler to QueuingHandler.
      l.addHandler(new QueueingHandler(accessLogHandler))
    }
  }

  def apply(req: Request, service: Service[Request, Response]): Future[Response] = {
    val requestIPAddress = req.xForwardedFor.getOrElse("-")
    val startTime = Time.now
    val requestMethod = req.method
    val requestHost = req.host.getOrElse("-")
    val requestPath = req.path.getOrDefault("-")
    val requestSessionId = req.cookies.getValue("border_session").fold("-")(c => c.takeRight(8))
    val requestContentLength = req.contentLength.getOrElse("-")
    for {
      resp <- service(req)
      _ <- Future(logger.log(accessLogLevel,
        /* IP Address */
        s"${requestIPAddress}\t"+
          /* Start Time */
          s"${startTime.format("[yyyy/MM/dd:hh:mm:ss.SSS]")}\t"+
          /* Reuest Method (GET/POST) */
          s"${requestMethod}\t"+
          /* Request Host */
          s"${requestHost}\t"+
          /* Request Path */
          s"${requestPath}\t"+
          /* Last 8 bytes of border_session cookie from request */
          s"${requestSessionId}\t"+
          /* Response Status Code */
          s"${resp.statusCode}\t"+
          /* Request Content length (in bytes) */
          s"${requestContentLength}\t"+
          /* Last 8 bytes of borser_session cookie from response */
          s"${resp.cookies.getValue("border_session").fold("-")(c => c.takeRight(8))}\t"+
          /* Response Content length (in bytes) */
          s"${resp.contentLength.getOrElse("-")}\t"+
          /* Latency (Time difference between request and response in milliSeconds) */
          s"${Time.now.since(startTime).inMilliseconds}\t"+
          /* Access Logs Version 1*/
          s"AccessLogV1"))
    } yield resp
  }
}
