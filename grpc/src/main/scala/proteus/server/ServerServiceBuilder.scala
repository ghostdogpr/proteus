package proteus
package server

import com.google.protobuf.Descriptors.FileDescriptor

case class ServerServiceBuilder[Unary[_], Streaming[_], Context, Rpcs] private (
  serverRpcs: List[ServerRpc[Unary, Streaming, Context, ?, ?]],
  dependencies: List[FileDescriptor] = Nil
)(
  using val backend: ServerBackend[Unary, Streaming, Context]
) {
  def rpc[Request, Response](
    rpc: Rpc.Unary[Request, Response],
    logic: Request => Unary[Response]
  ): ServerServiceBuilder[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerServiceBuilder(serverRpcs :+ server.ServerRpc.Unary(rpc, (req, _) => logic(req)))

  def rpc[Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    logic: Streaming[Request] => Unary[Response]
  ): ServerServiceBuilder[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerServiceBuilder(serverRpcs :+ server.ServerRpc.ClientStreaming(rpc, (req, _) => logic(req)))

  def rpc[Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    logic: Request => Streaming[Response]
  ): ServerServiceBuilder[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerServiceBuilder(serverRpcs :+ server.ServerRpc.ServerStreaming(rpc, (req, _) => logic(req)))

  def rpc[Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    logic: Streaming[Request] => Streaming[Response]
  ): ServerServiceBuilder[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerServiceBuilder(serverRpcs :+ server.ServerRpc.BidiStreaming(rpc, (req, _) => logic(req)))

  def rpcWithContext[Request, Response](
    rpc: Rpc.Unary[Request, Response],
    logic: (Request, Context) => Unary[Response]
  ): ServerServiceBuilder[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerServiceBuilder(serverRpcs :+ server.ServerRpc.Unary(rpc, logic(_, _)))

  def rpcWithContext[Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    logic: (Streaming[Request], Context) => Unary[Response]
  ): ServerServiceBuilder[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerServiceBuilder(serverRpcs :+ server.ServerRpc.ClientStreaming(rpc, logic(_, _)))

  def rpcWithContext[Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    logic: (Request, Context) => Streaming[Response]
  ): ServerServiceBuilder[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerServiceBuilder(serverRpcs :+ server.ServerRpc.ServerStreaming(rpc, logic(_, _)))

  def rpcWithContext[Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    logic: (Streaming[Request], Context) => Streaming[Response]
  ): ServerServiceBuilder[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerServiceBuilder(serverRpcs :+ server.ServerRpc.BidiStreaming(rpc, logic(_, _)))

  def dependsOn(dependency: Dependency*): ServerServiceBuilder[Unary, Streaming, Context, Rpcs] =
    copy(dependencies = dependencies ++ dependency.flatMap(_.fileDescriptor))

  def build(service: Service[Rpcs]): ServerService[Unary, Streaming, Context] =
    ServerService(service.dependsOn(dependencies), serverRpcs)(using backend)
}

object ServerServiceBuilder {
  def apply[Unary[_], Streaming[_], Context](
    using backend: ServerBackend[Unary, Streaming, Context]
  ): ServerServiceBuilder[Unary, Streaming, Context, Any] =
    ServerServiceBuilder(Nil)
}
