package proteus
package server

import io.grpc.{Metadata, ServerCall, ServerCallHandler, Status, StatusException, StatusRuntimeException}

/**
  * An interface for a server backend that can handle RPCs.
  * The backend is parameterized by the type of unary and streaming RPCs it can handle, and the context type for the server.
  */
trait ServerBackend[Unary[_], Streaming[_], Context] { self =>
  def handler[Request, Response](rpc: ServerRpc[Unary, Streaming, Context, Request, Response]): ServerCallHandler[Request, Response]
}

object ServerBackend {
  private[server] def closeCallWithError[Request, Response](call: ServerCall[Request, Response], ex: Throwable): Unit =
    ex match {
      case status: StatusException        =>
        call.close(status.getStatus, Option(status.getTrailers).getOrElse(new Metadata()))
      case status: StatusRuntimeException =>
        call.close(status.getStatus, Option(status.getTrailers).getOrElse(new Metadata()))
      case _                              =>
        call.close(Status.INTERNAL.withDescription(ex.getMessage).withCause(ex), new Metadata())
    }
}
