package proteus
package client

import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import fs2.Stream
import fs2.grpc.client.*
import io.grpc.*

def fs2Backend[F[_]: Async](channel: Channel, dispatcher: Dispatcher[F]): ClientBackend[F, Stream[F, *]] =
  new ClientBackend[F, Stream[F, *]] {
    import cats.syntax.all.*

    def client[Request, Response](service: Service[?], rpc: Rpc.Unary[Request, Response]): F[Request => F[Response]] = {
      val methodDescriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      Fs2ClientCall[F](channel, methodDescriptor, dispatcher, ClientOptions.default)
        .map(call => call.unaryToUnaryCall(_, new Metadata()))
    }

    def client[Request, Response](
      service: Service[?],
      rpc: Rpc.ClientStreaming[Request, Response]
    ): F[Stream[F, Request] => F[Response]] = {
      val methodDescriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      Fs2ClientCall[F](channel, methodDescriptor, dispatcher, ClientOptions.default)
        .map(call => call.streamingToUnaryCall(_, new Metadata()))
    }

    def client[Request, Response](
      service: Service[?],
      rpc: Rpc.ServerStreaming[Request, Response]
    ): F[Request => Stream[F, Response]] = {
      val methodDescriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      Fs2ClientCall[F](channel, methodDescriptor, dispatcher, ClientOptions.default)
        .map(call => call.unaryToStreamingCall(_, new Metadata()))
    }

    def client[Request, Response](
      service: Service[?],
      rpc: Rpc.BidiStreaming[Request, Response]
    ): F[Stream[F, Request] => Stream[F, Response]] = {
      val methodDescriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      Fs2ClientCall[F](channel, methodDescriptor, dispatcher, ClientOptions.default)
        .map(call => call.streamingToStreamingCall(_, new Metadata()))
    }

    def clientWithMetadata[Request, Response](
      service: Service[?],
      rpc: Rpc.Unary[Request, Response]
    ): F[(Request, Metadata) => F[(Response, Metadata)]] = {
      val methodDescriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      Fs2ClientCall[F](channel, methodDescriptor, dispatcher, ClientOptions.default)
        .map(call => call.unaryToUnaryCallTrailers(_, _))
    }

    def clientWithMetadata[Request, Response](
      service: Service[?],
      rpc: Rpc.ClientStreaming[Request, Response]
    ): F[(Stream[F, Request], Metadata) => F[(Response, Metadata)]] = {
      val methodDescriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      Fs2ClientCall[F](channel, methodDescriptor, dispatcher, ClientOptions.default)
        .map(call => call.streamingToUnaryCallTrailers(_, _))
    }

    def clientWithMetadata[Request, Response](
      service: Service[?],
      rpc: Rpc.ServerStreaming[Request, Response]
    ): F[(Request, Metadata) => Stream[F, Response]] = {
      val methodDescriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      Fs2ClientCall[F](channel, methodDescriptor, dispatcher, ClientOptions.default)
        .map(call => call.unaryToStreamingCall(_, _))
    }

    def clientWithMetadata[Request, Response](
      service: Service[?],
      rpc: Rpc.BidiStreaming[Request, Response]
    ): F[(Stream[F, Request], Metadata) => Stream[F, Response]] = {
      val methodDescriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      Fs2ClientCall[F](channel, methodDescriptor, dispatcher, ClientOptions.default)
        .map(call => call.streamingToStreamingCall(_, _))
    }
  }
