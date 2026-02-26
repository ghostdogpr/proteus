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
  def client[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[Request => F[Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.unaryToUnaryCall(_, new Metadata()))
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[Stream[F, Request] => F[Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.streamingToUnaryCall(_, new Metadata()))
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[Request => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.unaryToStreamingCall(_, new Metadata()))
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[Stream[F, Request] => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(call => call.streamingToStreamingCall(_, new Metadata()))
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[(Request, Metadata) => F[(Response, Metadata)]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(_.unaryToUnaryCallTrailers)
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[(Stream[F, Request], Metadata) => F[(Response, Metadata)]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(_.streamingToUnaryCallTrailers)
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[(Request, Metadata) => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(_.unaryToStreamingCall)
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): F[(Stream[F, Request], Metadata) => Stream[F, Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val clientOptions    = ClientOptions.default.configureCallOptions(options)
    Fs2ClientCall[F](channel, methodDescriptor, dispatcher, clientOptions)
      .map(_.streamingToStreamingCall)
  }
}

object Fs2ClientBackend {
  def apply[F[_]: Async](channel: Channel, dispatcher: Dispatcher[F]): Fs2ClientBackend[F] =
    new Fs2ClientBackend(channel, dispatcher)
}
