package proteus
package client

import io.grpc.*

trait ClientBackendUnary[Unary[_]] {
  def client[Request, Response](service: Service[?], rpc: Rpc.Unary[Request, Response]): Unary[Request => Unary[Response]]
  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.Unary[Request, Response]
  ): Unary[(Request, Metadata) => Unary[(Response, Metadata)]]
}

trait ClientBackend[Unary[_], Streaming[_]] extends ClientBackendUnary[Unary] {
  def client[Request, Response](service: Service[?], rpc: Rpc.ClientStreaming[Request, Response]): Unary[Streaming[Request] => Unary[Response]]
  def client[Request, Response](service: Service[?], rpc: Rpc.ServerStreaming[Request, Response]): Unary[Request => Streaming[Response]]
  def client[Request, Response](service: Service[?], rpc: Rpc.BidiStreaming[Request, Response]): Unary[Streaming[Request] => Streaming[Response]]
  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.ClientStreaming[Request, Response]
  ): Unary[(Streaming[Request], Metadata) => Unary[(Response, Metadata)]]
  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.ServerStreaming[Request, Response]
  ): Unary[(Request, Metadata) => Streaming[Response]]
  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.BidiStreaming[Request, Response]
  ): Unary[(Streaming[Request], Metadata) => Streaming[Response]]
}
