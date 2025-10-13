package proteus
package client

import io.grpc.*
import scalapb.zio_grpc.{SafeMetadata, ZChannel}
import scalapb.zio_grpc.client.ClientCalls
import zio.*
import zio.stream.*

class ZioClientBackend(channel: ZChannel) extends ClientBackend[IO[StatusException, *], ZStream[Any, StatusException, *]] {
  def client[Request, Response](
    service: Service[?],
    rpc: Rpc.Unary[Request, Response],
    options: CallOptions
  ): UIO[Request => IO[StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { req =>
      SafeMetadata.make.flatMap(headers => ClientCalls.unaryCall(channel, descriptor, options, headers, req))
    }
  }
  def client[Request, Response](
    service: Service[?],
    rpc: Rpc.ClientStreaming[Request, Response],
    options: CallOptions
  ): UIO[ZStream[Any, StatusException, Request] => IO[StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { req =>
      SafeMetadata.make.flatMap(headers => ClientCalls.clientStreamingCall(channel, descriptor, options, headers, req))
    }
  }
  def client[Request, Response](
    service: Service[?],
    rpc: Rpc.ServerStreaming[Request, Response],
    options: CallOptions
  ): UIO[Request => ZStream[Any, StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { req =>
      ZStream
        .fromZIO(SafeMetadata.make)
        .flatMap(headers => ClientCalls.serverStreamingCall(channel, descriptor, options, headers, req))
    }
  }
  def client[Request, Response](
    service: Service[?],
    rpc: Rpc.BidiStreaming[Request, Response],
    options: CallOptions
  ): UIO[ZStream[Any, StatusException, Request] => ZStream[Any, StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { req =>
      ZStream
        .fromZIO(SafeMetadata.make)
        .flatMap(headers => ClientCalls.bidiCall(channel, descriptor, options, headers, req))
    }
  }

  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.Unary[Request, Response],
    options: CallOptions
  ): UIO[(Request, Metadata) => IO[StatusException, (Response, Metadata)]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { (req, ctx) =>
      SafeMetadata
        .fromMetadata(ctx)
        .flatMap(headers =>
          ClientCalls.withMetadata.unaryCall(channel, descriptor, options, headers, req).map(resp => (resp.response, resp.trailers))
        )
    }
  }

  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.ClientStreaming[Request, Response],
    options: CallOptions
  ): UIO[(ZStream[Any, StatusException, Request], Metadata) => IO[StatusException, (Response, Metadata)]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { (req, ctx) =>
      SafeMetadata
        .fromMetadata(ctx)
        .flatMap(headers =>
          ClientCalls.withMetadata
            .clientStreamingCall(channel, descriptor, options, headers, req)
            .map(resp => (resp.response, resp.trailers))
        )
    }
  }

  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.ServerStreaming[Request, Response],
    options: CallOptions
  ): UIO[(Request, Metadata) => ZStream[Any, StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { (req, ctx) =>
      ZStream
        .fromZIO(SafeMetadata.fromMetadata(ctx))
        .flatMap(headers => ClientCalls.serverStreamingCall(channel, descriptor, options, headers, req))
    }
  }

  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.BidiStreaming[Request, Response],
    options: CallOptions
  ): UIO[(ZStream[Any, StatusException, Request], Metadata) => ZStream[Any, StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { (req, ctx) =>
      ZStream
        .fromZIO(SafeMetadata.fromMetadata(ctx))
        .flatMap(headers => ClientCalls.bidiCall(channel, descriptor, options, headers, req))
    }
  }
}
