package proteus
package client

import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fs2.Stream
import fs2.grpc.client.*
import io.grpc.*

/**
  * A client backend that wraps results in an abstract F[_] monad backed by Cats Effect typeclasses.
  * Streaming is supported using fs2 Stream.
  */
class Fs2ClientBackend[F[_]: Async](channel: Channel, dispatcher: Dispatcher[F]) extends ClientBackend[F, Stream[F, *]] {
  private def mkCall[Request, Response](
    methodDescriptor: MethodDescriptor[Request, Response],
    clientOptions: ClientOptions
  ): F[Fs2ClientCall[F, Request, Response]] =
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)

  def client[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[Request => F[Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Async[F].pure(request => mkCall(methodDescriptor, clientOptions).flatMap(_.unaryToUnaryCall(request, new Metadata())))
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[Stream[F, Request] => F[Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Async[F].pure(requests => mkCall(methodDescriptor, clientOptions).flatMap(_.streamingToUnaryCall(requests, new Metadata())))
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[Request => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Async[F].pure(request => Stream.eval(mkCall(methodDescriptor, clientOptions)).flatMap(call => call.unaryToStreamingCall(request, new Metadata())))
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[Stream[F, Request] => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Async[F].pure(requests => Stream.eval(mkCall(methodDescriptor, clientOptions)).flatMap(call => call.streamingToStreamingCall(requests, new Metadata())))
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[(Request, Metadata) => F[(Response, Metadata)]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Async[F].pure((request, metadata) => mkCall(methodDescriptor, clientOptions).flatMap(_.unaryToUnaryCallTrailers(request, metadata)))
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[(Stream[F, Request], Metadata) => F[(Response, Metadata)]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Async[F].pure((requests, metadata) => mkCall(methodDescriptor, clientOptions).flatMap(_.streamingToUnaryCallTrailers(requests, metadata)))
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[(Request, Metadata) => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Async[F].pure((request, metadata) => Stream.eval(mkCall(methodDescriptor, clientOptions)).flatMap(call => call.unaryToStreamingCall(request, metadata)))
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[(Stream[F, Request], Metadata) => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Async[F].pure((requests, metadata) => Stream.eval(mkCall(methodDescriptor, clientOptions)).flatMap(call => call.streamingToStreamingCall(requests, metadata)))
  }
}

object Fs2ClientBackend {
  def apply[F[_]: Async](channel: Channel, dispatcher: Dispatcher[F]): Fs2ClientBackend[F] =
    new Fs2ClientBackend(channel, dispatcher)
}
