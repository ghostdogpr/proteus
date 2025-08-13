package proteus
package server

import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import fs2.grpc.server.*
import io.grpc.*

import proteus.server.ServerBackend.RequestResponseMetadata
import proteus.server.ServerInterceptor

def fs2Backend[F[_]: Async](dispatcher: Dispatcher[F]): ServerBackend[F, [A] =>> _root_.fs2.Stream[F, A], RequestResponseMetadata] =
  fs2BackendWith(ServerInterceptor.empty, dispatcher)

def fs2BackendWith[F[_]: Async, Context](
  interceptor: ServerInterceptor[F, [A] =>> _root_.fs2.Stream[F, A], RequestResponseMetadata, Context],
  dispatcher: Dispatcher[F],
  serverOptions: ServerOptions = ServerOptions.default
): ServerBackend[F, [A] =>> _root_.fs2.Stream[F, A], Context] =
  new ServerBackend[F, [A] =>> _root_.fs2.Stream[F, A], Context] {
    import cats.syntax.all.*
    def handler[Request, Response](
      rpc: ServerRpc[F, [A] =>> _root_.fs2.Stream[F, A], Context, Request, Response]
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
