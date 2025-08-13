package proteus
package server

import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fs2.Stream
import fs2.grpc.server.*
import io.grpc.*

import proteus.server.ServerInterceptor

class Fs2ServerBackend[F[_]: Async, Context](
  interceptor: ServerInterceptor[F, Stream[F, *], RequestResponseMetadata, Context],
  dispatcher: Dispatcher[F],
  serverOptions: ServerOptions = ServerOptions.default
) extends ServerBackend[F, Stream[F, *], Context] {
  def handler[Request, Response](
    rpc: ServerRpc[F, Stream[F, *], Context, Request, Response]
  ): ServerCallHandler[Request, Response] =
    rpc match {
      case ServerRpc.Unary(_, logic)           =>
        Fs2ServerCallHandler[F](dispatcher, serverOptions).unaryToUnaryCallTrailers { (req, context) =>
          val responseMetadata = new Metadata()
          interceptor.unary(ctx => logic(req, ctx))(RequestResponseMetadata(context, responseMetadata)).map((_, responseMetadata))
        }
      case ServerRpc.ClientStreaming(_, logic) =>
        Fs2ServerCallHandler[F](dispatcher, serverOptions).streamingToUnaryCallTrailers { (req, context) =>
          val responseMetadata = new Metadata()
          interceptor.unary(ctx => logic(req, ctx))(RequestResponseMetadata(context, responseMetadata)).map((_, responseMetadata))
        }
      case ServerRpc.ServerStreaming(_, logic) =>
        Fs2ServerCallHandler[F](dispatcher, serverOptions).unaryToStreamingCall { (req, context) =>
          interceptor.stream(ctx => logic(req, ctx))(RequestResponseMetadata(context, new Metadata()))
        }
      case ServerRpc.BidiStreaming(_, logic)   =>
        Fs2ServerCallHandler[F](dispatcher, serverOptions).streamingToStreamingCall { (req, context) =>
          interceptor.stream(ctx => logic(req, ctx))(RequestResponseMetadata(context, new Metadata()))
        }
    }
}

object Fs2ServerBackend {
  def apply[F[_]: Async](
    dispatcher: Dispatcher[F],
    serverOptions: ServerOptions = ServerOptions.default
  ): Fs2ServerBackend[F, RequestResponseMetadata] =
    apply(ServerInterceptor.empty, dispatcher, serverOptions)

  def apply[F[_]: Async, Context](
    interceptor: ServerInterceptor[F, Stream[F, *], RequestResponseMetadata, Context],
    dispatcher: Dispatcher[F],
    serverOptions: ServerOptions
  ): Fs2ServerBackend[F, Context] =
    new Fs2ServerBackend(interceptor, dispatcher, serverOptions)
}
