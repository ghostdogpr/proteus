package proteus
package server

import io.grpc.{Metadata, ServerCall, Status}

/**
  * A `ServerCall.Listener` base class for RPCs with a single (unary) request.
  *
  * Implements the gRPC protocol contract: exactly one `onMessage` before
  * `onHalfClose`. A protocol-violating client that sends zero or more than one
  * message has the call closed with `Status.INVALID_ARGUMENT` before the user
  * handler is invoked.
  *
  * Subclasses provide [[onRequest]] (invoked when the half-close arrives with
  * a valid single request) and may override [[onCallCancelled]] and
  * [[onCallReady]] for cancellation / back-pressure plumbing.
  *
  * Thread-safety: gRPC's serializing executor invokes listener callbacks with
  * happens-before between invocations, so plain `var`s suffice for state held
  * across `onMessage` / `onHalfClose`.
  */
abstract class UnaryInputListener[Request, Response](
  call: ServerCall[Request, Response]
) extends ServerCall.Listener[Request] {

  private var request: Request | Null = null
  private var received: Int           = 0

  protected def onRequest(req: Request): Unit

  protected def onCallCancelled(): Unit = ()

  protected def onCallReady(): Unit = ()

  final override def onMessage(message: Request): Unit = {
    received += 1
    request = message
  }

  final override def onHalfClose(): Unit =
    if (received != 1 || request == null) {
      val desc = if (received == 0) "Half-closed without a request" else s"Expected 1 request, got $received"
      call.close(Status.INVALID_ARGUMENT.withDescription(desc), new Metadata())
    } else onRequest(request.asInstanceOf[Request])

  final override def onCancel(): Unit = onCallCancelled()

  final override def onReady(): Unit = onCallReady()
}
