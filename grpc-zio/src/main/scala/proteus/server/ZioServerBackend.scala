package proteus
package server

import io.grpc.*
import scalapb.zio_grpc.RequestContext
import scalapb.zio_grpc.server.ZServerCallHandler
import zio.*
import zio.stream.*

import proteus.server.ServerInterceptor

/**
  * A server backend that wraps results in a ZIO effect.
  * Streaming is supported using ZStream.
  *
  * @param interceptor an interceptor that can run on every request.
  * @param runtime the ZIO runtime to use for running ZIO effects.
  */
class ZioServerBackend[R, E, Context](
  interceptor: ServerInterceptor[IO[StatusException, *], ZIO[R, E, *], ZStream[Any, StatusException, *], ZStream[R, E, *], RequestContext, Context],
  runtime: Runtime[Any] = Runtime.default
) extends ServerBackend[ZIO[R, E, *], ZStream[R, E, *], Context] {
  def handler[Request, Response](
    rpc: ServerRpc[ZIO[R, E, *], ZStream[R, E, *], Context, Request, Response]
  ): ServerCallHandler[Request, Response] =
    rpc match {
      case ServerRpc.Unary(rpc, logic)           =>
        ZServerCallHandler.unaryCallHandler(
          runtime,
          (req, context) => interceptor.unary(ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(req)(context)
        )
      case ServerRpc.ClientStreaming(rpc, logic) =>
        ZServerCallHandler.clientStreamingCallHandler(
          runtime,
          (req, context) =>
            interceptor.clientStreaming[Request, Response](req => ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(req)(context)
        )
      case ServerRpc.ServerStreaming(rpc, logic) =>
        ZServerCallHandler.serverStreamingCallHandler(
          runtime,
          (req, context) => interceptor.serverStreaming(ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(req)(context)
        )
      case ServerRpc.BidiStreaming(rpc, logic)   =>
        ZServerCallHandler.bidiCallHandler(
          runtime,
          (req, context) =>
            interceptor.bidiStreaming[Request, Response](req => ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(req)(context)
        )
    }
}

/**
  * A server backend that wraps results in a ZIO effect.
  * Streaming is supported using ZStream.
  * This backend doesn't have any interceptors and uses the default ZIO runtime.
  */
object ZioServerBackend extends ZioServerBackend(ServerInterceptor.empty, Runtime.default)
