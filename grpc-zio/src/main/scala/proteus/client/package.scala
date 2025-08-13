package proteus
package client

import io.grpc.*
import scalapb.zio_grpc.{SafeMetadata, ZChannel}
import scalapb.zio_grpc.client.ClientCalls
import zio.*
import zio.stream.*

def zioBackend(channel: ZChannel): ClientBackend[IO[StatusException, *], ZStream[Any, StatusException, *]] =
  new ClientBackend[IO[StatusException, *], ZStream[Any, StatusException, *]] {
    def client[Request, Response](service: Service[?], rpc: Rpc.Unary[Request, Response]): UIO[Request => IO[StatusException, Response]] = {
      val descriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      ZIO.succeed { req =>
        SafeMetadata.make.flatMap(headers => ClientCalls.unaryCall(channel, descriptor, CallOptions.DEFAULT, headers, req))
      }
    }
    def client[Request, Response](
      service: Service[?],
      rpc: Rpc.ClientStreaming[Request, Response]
    ): UIO[ZStream[Any, StatusException, Request] => IO[StatusException, Response]] = {
      val descriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      ZIO.succeed { req =>
        SafeMetadata.make.flatMap(headers => ClientCalls.clientStreamingCall(channel, descriptor, CallOptions.DEFAULT, headers, req))
      }
    }
    def client[Request, Response](
      service: Service[?],
      rpc: Rpc.ServerStreaming[Request, Response]
    ): UIO[Request => ZStream[Any, StatusException, Response]] = {
      val descriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      ZIO.succeed { req =>
        ZStream
          .fromZIO(SafeMetadata.make)
          .flatMap(headers => ClientCalls.serverStreamingCall(channel, descriptor, CallOptions.DEFAULT, headers, req))
      }
    }
    def client[Request, Response](
      service: Service[?],
      rpc: Rpc.BidiStreaming[Request, Response]
    ): UIO[ZStream[Any, StatusException, Request] => ZStream[Any, StatusException, Response]] = {
      val descriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      ZIO.succeed { req =>
        ZStream
          .fromZIO(SafeMetadata.make)
          .flatMap(headers => ClientCalls.bidiCall(channel, descriptor, CallOptions.DEFAULT, headers, req))
      }
    }

    def clientWithMetadata[Request, Response](
      service: Service[?],
      rpc: Rpc.Unary[Request, Response]
    ): UIO[(Request, Metadata) => IO[StatusException, (Response, Metadata)]] = {
      val descriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      ZIO.succeed { (req, ctx) =>
        SafeMetadata
          .fromMetadata(ctx)
          .flatMap(headers =>
            ClientCalls.withMetadata.unaryCall(channel, descriptor, CallOptions.DEFAULT, headers, req).map(resp => (resp.response, resp.trailers))
          )
      }
    }

    def clientWithMetadata[Request, Response](
      service: Service[?],
      rpc: Rpc.ClientStreaming[Request, Response]
    ): UIO[(ZStream[Any, StatusException, Request], Metadata) => IO[StatusException, (Response, Metadata)]] = {
      val descriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      ZIO.succeed { (req, ctx) =>
        SafeMetadata
          .fromMetadata(ctx)
          .flatMap(headers =>
            ClientCalls.withMetadata
              .clientStreamingCall(channel, descriptor, CallOptions.DEFAULT, headers, req)
              .map(resp => (resp.response, resp.trailers))
          )
      }
    }

    def clientWithMetadata[Request, Response](
      service: Service[?],
      rpc: Rpc.ServerStreaming[Request, Response]
    ): UIO[(Request, Metadata) => ZStream[Any, StatusException, Response]] = {
      val descriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      ZIO.succeed { (req, ctx) =>
        ZStream
          .fromZIO(SafeMetadata.fromMetadata(ctx))
          .flatMap(headers => ClientCalls.serverStreamingCall(channel, descriptor, CallOptions.DEFAULT, headers, req))
      }
    }

    def clientWithMetadata[Request, Response](
      service: Service[?],
      rpc: Rpc.BidiStreaming[Request, Response]
    ): UIO[(ZStream[Any, StatusException, Request], Metadata) => ZStream[Any, StatusException, Response]] = {
      val descriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      ZIO.succeed { (req, ctx) =>
        ZStream
          .fromZIO(SafeMetadata.fromMetadata(ctx))
          .flatMap(headers => ClientCalls.bidiCall(channel, descriptor, CallOptions.DEFAULT, headers, req))
      }
    }
  }
