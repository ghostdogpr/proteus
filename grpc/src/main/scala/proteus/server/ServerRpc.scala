package proteus
package server

trait ServerRpc[Unary[_], Streaming[_], Context, Req, Resp](rpc: Rpc[Req, Resp]) {
  export rpc.*
}

object ServerRpc {
  case class Unary[Unary[_], Streaming[_], Context, Req, Resp](rpc: Rpc[Req, Resp], logic: (Req, Context) => Unary[Resp])
    extends ServerRpc[Unary, Streaming, Context, Req, Resp](rpc)
  case class ClientStreaming[Unary[_], Streaming[_], Context, Req, Resp](rpc: Rpc[Req, Resp], logic: (Streaming[Req], Context) => Unary[Resp])
    extends ServerRpc[Unary, Streaming, Context, Req, Resp](rpc)
  case class ServerStreaming[Unary[_], Streaming[_], Context, Req, Resp](rpc: Rpc[Req, Resp], logic: (Req, Context) => Streaming[Resp])
    extends ServerRpc[Unary, Streaming, Context, Req, Resp](rpc)
  case class BidiStreaming[Unary[_], Streaming[_], Context, Req, Resp](rpc: Rpc[Req, Resp], logic: (Streaming[Req], Context) => Streaming[Resp])
    extends ServerRpc[Unary, Streaming, Context, Req, Resp](rpc)
}
