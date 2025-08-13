package proteus
package server

import io.grpc.ServerCallHandler

trait ServerBackend[Unary[_], Streaming[_], Context] { self =>
  def handler[Request, Response](rpc: ServerRpc[Unary, Streaming, Context, Request, Response]): ServerCallHandler[Request, Response]
}
