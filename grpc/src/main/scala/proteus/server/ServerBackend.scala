package proteus
package server

import io.grpc.{Metadata, ServerCall, ServerCallHandler, Status, StatusException, StatusRuntimeException}

/**
  * An interface for a server backend that can handle RPCs.
  * The backend is parameterized by the type of unary and streaming RPCs it can handle, and the context type for the server.
  */
trait ServerBackend[Unary[_], Streaming[_], Context] { self =>
  type Tag[A]

  def handler[Request, Response](rpc: ServerRpc[Unary, Streaming, Tag, Context, Request, Response]): ServerCallHandler[Request, Response]
}

object ServerBackend {
  private[server] def sendUnaryResponse[Response](call: ServerCall[?, Response], response: Response): Unit = {
    call.sendHeaders(new Metadata())
    call.sendMessage(response)
  }

  private[server] def closeCallWithError[Request, Response](call: ServerCall[Request, Response], ex: Throwable): Unit =
    closeCallWithError(call, ex, new Metadata())

  /**
    * Merges any `StatusException` trailers into `responseMetadata` (preserving user-set headers) before closing.
    */
  private[server] def closeCallWithError[Request, Response](
    call: ServerCall[Request, Response],
    ex: Throwable,
    responseMetadata: Metadata
  ): Unit = {
    val (status, trailers) = ex match {
      case e: StatusException        => (e.getStatus, e.getTrailers)
      case e: StatusRuntimeException => (e.getStatus, e.getTrailers)
      case e                         => (Status.INTERNAL.withDescription(e.getMessage).withCause(e), null)
    }
    if (trailers != null) responseMetadata.merge(trailers)
    call.close(status, responseMetadata)
  }
}
