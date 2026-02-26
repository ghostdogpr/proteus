package proteus
package client

import io.grpc.*
import scalapb.zio_grpc.{SafeMetadata, ZChannel}
import scalapb.zio_grpc.client.ClientCalls
import zio.*
import zio.stream.*

/**
  * A client backend that wraps results in a ZIO effect.
  * Streaming is supported using ZStream.
  */
class ZioClientBackend(channel: ZChannel) extends ClientBackend[IO[StatusException, *], ZStream[Any, StatusException, *]] {
  def client[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): UIO[Request => IO[StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { req =>
      SafeMetadata.make.flatMap(headers => ClientCalls.unaryCall(channel, descriptor, options(CallOptions.DEFAULT), headers, req))
    }
  }
  def client[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): UIO[ZStream[Any, StatusException, Request] => IO[StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { req =>
      SafeMetadata.make.flatMap(headers => ClientCalls.clientStreamingCall(channel, descriptor, options(CallOptions.DEFAULT), headers, req))
    }
  }
  def client[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): UIO[Request => ZStream[Any, StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { req =>
      ZStream
        .fromZIO(SafeMetadata.make)
        .flatMap(headers => ClientCalls.serverStreamingCall(channel, descriptor, options(CallOptions.DEFAULT), headers, req))
    }
  }
  def client[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): UIO[ZStream[Any, StatusException, Request] => ZStream[Any, StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { req =>
      ZStream
        .fromZIO(SafeMetadata.make)
        .flatMap(headers => ClientCalls.bidiCall(channel, descriptor, options(CallOptions.DEFAULT), headers, req))
    }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): UIO[(Request, Metadata) => IO[StatusException, (Response, Metadata)]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { (req, ctx) =>
      SafeMetadata
        .fromMetadata(ctx)
        .flatMap(headers =>
          ClientCalls.withMetadata
            .unaryCall(channel, descriptor, options(CallOptions.DEFAULT), headers, req)
            .map(resp => (resp.response, resp.trailers))
        )
    }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): UIO[(ZStream[Any, StatusException, Request], Metadata) => IO[StatusException, (Response, Metadata)]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { (req, ctx) =>
      SafeMetadata
        .fromMetadata(ctx)
        .flatMap(headers =>
          ClientCalls.withMetadata
            .clientStreamingCall(channel, descriptor, options(CallOptions.DEFAULT), headers, req)
            .map(resp => (resp.response, resp.trailers))
        )
    }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): UIO[(Request, Metadata) => ZStream[Any, StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { (req, ctx) =>
      ZStream
        .fromZIO(SafeMetadata.fromMetadata(ctx))
        .flatMap(headers => ClientCalls.serverStreamingCall(channel, descriptor, options(CallOptions.DEFAULT), headers, req))
    }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): UIO[(ZStream[Any, StatusException, Request], Metadata) => ZStream[Any, StatusException, Response]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    ZIO.succeed { (req, ctx) =>
      ZStream
        .fromZIO(SafeMetadata.fromMetadata(ctx))
        .flatMap(headers => ClientCalls.bidiCall(channel, descriptor, options(CallOptions.DEFAULT), headers, req))
    }
  }
}
