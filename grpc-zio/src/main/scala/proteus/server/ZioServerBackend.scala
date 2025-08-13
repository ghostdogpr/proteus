package proteus
package server

import io.grpc.*
import scalapb.zio_grpc.RequestContext
import scalapb.zio_grpc.server.ZServerCallHandler
import zio.*
import zio.stream.*

import proteus.server.ServerInterceptor

class ZioServerBackend[Context](
  interceptor: ServerInterceptor[IO[StatusException, *], ZStream[Any, StatusException, *], RequestContext, Context],
  runtime: Runtime[Any] = Runtime.default
) extends ServerBackend[IO[StatusException, *], ZStream[Any, StatusException, *], Context] {
  def handler[Request, Response](
    rpc: ServerRpc[IO[StatusException, *], ZStream[Any, StatusException, *], Context, Request, Response]
  ): ServerCallHandler[Request, Response] =
    rpc match {
      case ServerRpc.Unary(_, logic)           =>
        ZServerCallHandler.unaryCallHandler(runtime, (req, context) => interceptor.unary(ctx => logic(req, ctx))(context))
      case ServerRpc.ClientStreaming(_, logic) =>
        ZServerCallHandler.clientStreamingCallHandler(runtime, (req, context) => interceptor.unary(ctx => logic(req, ctx))(context))
      case ServerRpc.ServerStreaming(_, logic) =>
        ZServerCallHandler.serverStreamingCallHandler(runtime, (req, context) => interceptor.stream(ctx => logic(req, ctx))(context))
      case ServerRpc.BidiStreaming(_, logic)   =>
        ZServerCallHandler.bidiCallHandler(runtime, (req, context) => interceptor.stream(ctx => logic(req, ctx))(context))
    }
}

object ZioServerBackend {
  def apply: ZioServerBackend[RequestContext] =
    new ZioServerBackend(ServerInterceptor.empty)
}
