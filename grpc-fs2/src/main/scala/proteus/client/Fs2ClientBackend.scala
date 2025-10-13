package proteus
package client

import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fs2.Stream
import fs2.grpc.client.*
import io.grpc.*

class Fs2ClientBackend[F[_]: Async](channel: Channel, dispatcher: Dispatcher[F]) extends ClientBackend[F, Stream[F, *]] {
  def client[Request, Response](service: Service[?], rpc: Rpc.Unary[Request, Response], options: CallOptions): F[Request => F[Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(_ => options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.unaryToUnaryCall(_, new Metadata()))
  }

  def client[Request, Response](
    service: Service[?],
    rpc: Rpc.ClientStreaming[Request, Response],
    options: CallOptions
  ): F[Stream[F, Request] => F[Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(_ => options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.streamingToUnaryCall(_, new Metadata()))
  }

  def client[Request, Response](
    service: Service[?],
    rpc: Rpc.ServerStreaming[Request, Response],
    options: CallOptions
  ): F[Request => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(_ => options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.unaryToStreamingCall(_, new Metadata()))
  }

  def client[Request, Response](
    service: Service[?],
    rpc: Rpc.BidiStreaming[Request, Response],
    options: CallOptions
  ): F[Stream[F, Request] => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(_ => options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.streamingToStreamingCall(_, new Metadata()))
  }

  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.Unary[Request, Response],
    options: CallOptions
  ): F[(Request, Metadata) => F[(Response, Metadata)]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(_ => options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.unaryToUnaryCallTrailers(_, _))
  }

  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.ClientStreaming[Request, Response],
    options: CallOptions
  ): F[(Stream[F, Request], Metadata) => F[(Response, Metadata)]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(_ => options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.streamingToUnaryCallTrailers(_, _))
  }

  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.ServerStreaming[Request, Response],
    options: CallOptions
  ): F[(Request, Metadata) => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(_ => options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.unaryToStreamingCall(_, _))
  }

  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.BidiStreaming[Request, Response],
    options: CallOptions
  ): F[(Stream[F, Request], Metadata) => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(_ => options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.streamingToStreamingCall(_, _))
  }
}

object Fs2ClientBackend {
  def apply[F[_]: Async](channel: Channel, dispatcher: Dispatcher[F]): Fs2ClientBackend[F] =
    new Fs2ClientBackend(channel, dispatcher)
}
