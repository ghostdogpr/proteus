package proteus
package server

import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fs2.Stream
import fs2.grpc.server.*
import io.grpc.*

import proteus.server.ServerInterceptor

/**
  * A server backend that wraps results in an abstract F[_] monad backed by Cats Effect typeclasses.
  * Streaming is supported using Fs2 Stream.
  *
  * @param interceptor an interceptor that can run on every request.
  */
class Fs2ServerBackend[F[_]: Async, G[_], Context](
  interceptor: ServerInterceptor[F, G, Stream[F, *], Stream[G, *], RequestResponseMetadata, Context],
  dispatcher: Dispatcher[F],
  serverOptions: ServerOptions = ServerOptions.default
) extends ServerBackend[G, Stream[G, *], Context] {
  def handler[Request, Response](
    rpc: ServerRpc[G, Stream[G, *], Context, Request, Response]
  ): ServerCallHandler[Request, Response] =
    rpc match {
      case ServerRpc.Unary(rpc, logic)           =>
        Fs2ServerCallHandler[F](dispatcher, serverOptions).unaryToUnaryCallTrailers { (req, context) =>
          val responseMetadata = new Metadata()
          interceptor
            .unary(ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(req)(RequestResponseMetadata(context, responseMetadata))
            .map((_, responseMetadata))
        }
      case ServerRpc.ClientStreaming(rpc, logic) =>
        Fs2ServerCallHandler[F](dispatcher, serverOptions).streamingToUnaryCallTrailers { (req, context) =>
          val responseMetadata = new Metadata()
          interceptor
            .clientStreaming[Request, Response](req => ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(req)(
              RequestResponseMetadata(context, responseMetadata)
            )
            .map((_, responseMetadata))
        }
      case ServerRpc.ServerStreaming(rpc, logic) =>
        Fs2ServerCallHandler[F](dispatcher, serverOptions).unaryToStreamingCall { (req, context) =>
          interceptor.serverStreaming(ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(req)(
            RequestResponseMetadata(context, new Metadata())
          )
        }
      case ServerRpc.BidiStreaming(rpc, logic)   =>
        Fs2ServerCallHandler[F](dispatcher, serverOptions).streamingToStreamingCall { (req, context) =>
          interceptor.bidiStreaming[Request, Response](req => ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(req)(
            RequestResponseMetadata(context, new Metadata())
          )
        }
    }
}

object Fs2ServerBackend {

  /**
    * Creates a new fs2 server backend that wraps results in an abstract F[_] monad backed by Cats Effect typeclasses.
    * Streaming is supported using Fs2 Stream.
    *
    * @param dispatcher a Cats Effect dispatcher.
    * @param serverOptions the options to use for the server.
    */
  def apply[F[_]: Async](
    dispatcher: Dispatcher[F],
    serverOptions: ServerOptions = ServerOptions.default
  ): Fs2ServerBackend[F, F, RequestResponseMetadata] =
    apply(ServerInterceptor.empty, dispatcher, serverOptions)

  /**
    * Creates a new fs2 server backend that wraps results in an abstract F[_] monad backed by Cats Effect typeclasses.
    * Streaming is supported using Fs2 Stream.
    *
    * @param interceptor an interceptor that can run on every request.
    * @param dispatcher a Cats Effect dispatcher.
    * @param serverOptions the options to use for the server.
    */
  def apply[F[_]: Async, Context](
    interceptor: ServerContextInterceptor[F, Stream[F, *], RequestResponseMetadata, Context],
    dispatcher: Dispatcher[F],
    serverOptions: ServerOptions
  ): Fs2ServerBackend[F, F, Context] =
    new Fs2ServerBackend(interceptor, dispatcher, serverOptions)
}
