package proteus
package server

import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import fs2.Stream
import fs2.grpc.server.*
import io.grpc.*

import proteus.server.ServerBackend.RequestResponseMetadata
import proteus.server.ServerInterceptor

def fs2Backend[F[_]: Async](dispatcher: Dispatcher[F]): ServerBackend[F, Stream[F, *], RequestResponseMetadata] =
  fs2BackendWith(ServerInterceptor.empty, dispatcher)

def fs2BackendWith[F[_]: Async, Context](
  interceptor: ServerInterceptor[F, Stream[F, *], RequestResponseMetadata, Context],
  dispatcher: Dispatcher[F],
  serverOptions: ServerOptions = ServerOptions.default
): ServerBackend[F, Stream[F, *], Context] =
  new ServerBackend[F, Stream[F, *], Context] {
    import cats.syntax.all.*
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
