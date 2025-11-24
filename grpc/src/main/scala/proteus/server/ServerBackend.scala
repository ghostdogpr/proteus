package proteus
package server

import io.grpc.ServerCallHandler

/**
  * An interface for a server backend that can handle RPCs.
  * The backend is parameterized by the type of unary and streaming RPCs it can handle, and the context type for the server.
  */
trait ServerBackend[Unary[_], Streaming[_], Context] { self =>
  def handler[Request, Response](rpc: ServerRpc[Unary, Streaming, Context, Request, Response]): ServerCallHandler[Request, Response]
}
