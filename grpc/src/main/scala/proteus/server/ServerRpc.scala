package proteus
package server

/**
  * A server RPC definition, which includes the RPC definition and the logic to handle the request.
  */
trait ServerRpc[Unary[_], Streaming[_], Tag[_], Context, Req, Resp](rpc: Rpc[Req, Resp]) {
  export rpc.*
}

object ServerRpc {

  /**
    * A unary server RPC definition, which includes the RPC definition and the logic to handle the request.
    */
  case class Unary[Unary[_], Streaming[_], Tag[_], Context, Req, Resp](rpc: Rpc[Req, Resp], logic: (Req, Context) => Unary[Resp])
    extends ServerRpc[Unary, Streaming, Tag, Context, Req, Resp](rpc)

  /**
    * A client streaming server RPC definition, which includes the RPC definition and the logic to handle the request.
    */
  case class ClientStreaming[Unary[_], Streaming[_], Tag[_], Context, Req, Resp](
    rpc: Rpc[Req, Resp],
    logic: (Streaming[Req], Context) => Unary[Resp],
    reqTag: Tag[Req]
  ) extends ServerRpc[Unary, Streaming, Tag, Context, Req, Resp](rpc)

  /**
    * A server streaming server RPC definition, which includes the RPC definition and the logic to handle the request.
    */
  case class ServerStreaming[Unary[_], Streaming[_], Tag[_], Context, Req, Resp](
    rpc: Rpc[Req, Resp],
    logic: (Req, Context) => Streaming[Resp],
    respTag: Tag[Resp]
  ) extends ServerRpc[Unary, Streaming, Tag, Context, Req, Resp](rpc)

  /**
    * A bidirectional streaming server RPC definition, which includes the RPC definition and the logic to handle the request.
    */
  case class BidiStreaming[Unary[_], Streaming[_], Tag[_], Context, Req, Resp](
    rpc: Rpc[Req, Resp],
    logic: (Streaming[Req], Context) => Streaming[Resp],
    reqTag: Tag[Req],
    respTag: Tag[Resp]
  ) extends ServerRpc[Unary, Streaming, Tag, Context, Req, Resp](rpc)
}
