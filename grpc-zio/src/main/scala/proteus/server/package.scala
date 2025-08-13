package proteus
package server

import io.grpc.*
import scalapb.zio_grpc.RequestContext
import scalapb.zio_grpc.server.ZServerCallHandler
import zio.*
import zio.stream.*

import proteus.server.ServerInterceptor

val zioBackend: ServerBackend[[A] =>> IO[StatusException, A], [A] =>> ZStream[Any, StatusException, A], RequestContext] =
  zioBackendWith(ServerInterceptor.empty)

def zioBackendWith[Context](
  interceptor: ServerInterceptor[[A] =>> A, [A] =>> A, RequestContext, Context],
  runtime: Runtime[Any] = Runtime.default
): ServerBackend[[A] =>> IO[StatusException, A], [A] =>> ZStream[Any, StatusException, A], Context] =
  new ServerBackend[[A] =>> IO[StatusException, A], [A] =>> ZStream[Any, StatusException, A], Context] {
    def handler[Request, Response](
      rpc: ServerRpc[[A] =>> IO[StatusException, A], [A] =>> ZStream[Any, StatusException, A], Context, Request, Response]
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
