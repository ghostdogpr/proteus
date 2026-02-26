package proteus
package client

import io.grpc.*

/**
  * An interface for a client backend that supports unary RPCs.
  *
  * @param Unary the type returned by the non-streaming client methods.
  */
trait ClientBackendUnary[Unary[_]] {

  /**
    * Creates a new unary client.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    */
  def client[Rpcs, Request, Response](rpc: Rpc.Unary[Request, Response], service: Service[Rpcs])(
    using HasRpc[Rpcs, rpc.type]
  ): Unary[Request => Unary[Response]] =
    client(rpc, service, identity)

  /**
    * Creates a new unary client.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    * @param options the options to use for the client.
    */
  def client[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Unary[Request => Unary[Response]]

  /**
    * Creates a new unary client with the ability to send and receive metadata.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    */
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs]
  )(using HasRpc[Rpcs, rpc.type]): Unary[(Request, Metadata) => Unary[(Response, Metadata)]] =
    clientWithMetadata(rpc, service, identity)

  /**
    * Creates a new unary client with the ability to send and receive metadata.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    * @param options the options to use for the client.
    */
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions = identity
  )(using HasRpc[Rpcs, rpc.type]): Unary[(Request, Metadata) => Unary[(Response, Metadata)]]
}

/**
  * An interface for a client backend that supports streaming RPCs.
  *
  * @param Unary the type returned by the non-streaming client methods.
  * @param Streaming the type returned by the streaming client methods.
  */
trait ClientBackend[Unary[_], Streaming[_]] extends ClientBackendUnary[Unary] {

  /**
    * Creates a new client streaming client.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    */
  def client[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs]
  )(using HasRpc[Rpcs, rpc.type]): Unary[Streaming[Request] => Unary[Response]] =
    client(rpc, service, identity)

  /**
    * Creates a new client streaming client.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    * @param options the options to use for the client.
    */
  def client[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Unary[Streaming[Request] => Unary[Response]]

  /**
    * Creates a new server streaming client.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    */
  def client[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs]
  )(using HasRpc[Rpcs, rpc.type]): Unary[Request => Streaming[Response]] = client(rpc, service, identity)

  /**
    * Creates a new server streaming client.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    * @param options the options to use for the client.
    */
  def client[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Unary[Request => Streaming[Response]]

  /**
    * Creates a new bidirectional streaming client.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    */
  def client[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs]
  )(using HasRpc[Rpcs, rpc.type]): Unary[Streaming[Request] => Streaming[Response]] = client(rpc, service, identity)

  /**
    * Creates a new bidirectional streaming client.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    * @param options the options to use for the client.
    */
  def client[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Unary[Streaming[Request] => Streaming[Response]]

  /**
    * Creates a new client streaming client with the ability to send and receive metadata.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    */
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs]
  )(using HasRpc[Rpcs, rpc.type]): Unary[(Streaming[Request], Metadata) => Unary[(Response, Metadata)]] = clientWithMetadata(rpc, service, identity)

  /**
    * Creates a new client streaming client with the ability to send and receive metadata.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    * @param options the options to use for the client.
    */
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Unary[(Streaming[Request], Metadata) => Unary[(Response, Metadata)]]

  /**
    * Creates a new server streaming client with the ability to send and receive metadata.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    */
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs]
  )(using HasRpc[Rpcs, rpc.type]): Unary[(Request, Metadata) => Streaming[Response]] = clientWithMetadata(rpc, service, identity)

  /**
    * Creates a new server streaming client with the ability to send and receive metadata.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    * @param options the options to use for the client.
    */
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Unary[(Request, Metadata) => Streaming[Response]]

  /**
    * Creates a new bidirectional streaming client with the ability to send and receive metadata.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    */
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs]
  )(using HasRpc[Rpcs, rpc.type]): Unary[(Streaming[Request], Metadata) => Streaming[Response]] = clientWithMetadata(rpc, service, identity)

  /**
    * Creates a new bidirectional streaming client with the ability to send and receive metadata.
    *
    * @param rpc the RPC to create the client for.
    * @param service the service to create the client for.
    * @param options the options to use for the client.
    */
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Unary[(Streaming[Request], Metadata) => Streaming[Response]]
}
