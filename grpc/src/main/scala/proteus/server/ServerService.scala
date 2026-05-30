package proteus
package server

import com.google.protobuf.Descriptors.FileDescriptor
import io.grpc.*
import io.grpc.protobuf.ProtoFileDescriptorSupplier

/**
  * A server service definition, which includes the RPCs of the service with their logic and the backend to handle the requests.
  *
  * @param serverRpcs the RPCs of the service with their logic.
  * @param backend the backend to handle the requests.
  */
class ServerService[Unary[_], Streaming[_], Context, Rpcs, Backend <: ServerBackend[Unary, Streaming, Context]] private (
  val backend: Backend,
  serverRpcs: List[ServerRpc[Unary, Streaming, backend.Tag, Context, ?, ?]]
) {

  /**
    * Provides the logic for the given unary RPC.
    *
    * @param rpc the RPC definition.
    * @param logic the logic to handle the request.
    */
  def rpc[Request, Response](
    rpc: Rpc.Unary[Request, Response],
    logic: Request => Unary[Response]
  ): ServerService[Unary, Streaming, Context, Rpcs & rpc.type, Backend] =
    ServerService(
      backend,
      serverRpcs :+ server.ServerRpc.Unary[Unary, Streaming, backend.Tag, Context, Request, Response](rpc, (req, _) => logic(req))
    )

  /**
    * Provides the logic for the given client streaming RPC.
    *
    * @param rpc the RPC definition.
    * @param logic the logic to handle the request.
    */
  def rpc[Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    logic: Streaming[Request] => Unary[Response]
  )(using reqTag: backend.Tag[Request]): ServerService[Unary, Streaming, Context, Rpcs & rpc.type, Backend] =
    ServerService(
      backend,
      serverRpcs :+ server.ServerRpc.ClientStreaming[Unary, Streaming, backend.Tag, Context, Request, Response](rpc, (req, _) => logic(req), reqTag)
    )

  /**
    * Provides the logic for the given server streaming RPC.
    *
    * @param rpc the RPC definition.
    * @param logic the logic to handle the request.
    */
  def rpc[Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    logic: Request => Streaming[Response]
  )(using respTag: backend.Tag[Response]): ServerService[Unary, Streaming, Context, Rpcs & rpc.type, Backend] =
    ServerService(
      backend,
      serverRpcs :+ server.ServerRpc.ServerStreaming[Unary, Streaming, backend.Tag, Context, Request, Response](rpc, (req, _) => logic(req), respTag)
    )

  /**
    * Provides the logic for the given bidirectional streaming RPC.
    *
    * @param rpc the RPC definition.
    * @param logic the logic to handle the request.
    */
  def rpc[Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    logic: Streaming[Request] => Streaming[Response]
  )(using reqTag: backend.Tag[Request], respTag: backend.Tag[Response]): ServerService[Unary, Streaming, Context, Rpcs & rpc.type, Backend] =
    ServerService(
      backend,
      serverRpcs :+ server.ServerRpc.BidiStreaming[Unary, Streaming, backend.Tag, Context, Request, Response](
        rpc,
        (req, _) => logic(req),
        reqTag,
        respTag
      )
    )

  /**
    * Provides the logic for the given unary RPC, also receiving the context.
    *
    * @param rpc the RPC definition.
    * @param logic the logic to handle the request.
    */
  def rpcWithContext[Request, Response](
    rpc: Rpc.Unary[Request, Response],
    logic: (Request, Context) => Unary[Response]
  ): ServerService[Unary, Streaming, Context, Rpcs & rpc.type, Backend] =
    ServerService(backend, serverRpcs :+ server.ServerRpc.Unary[Unary, Streaming, backend.Tag, Context, Request, Response](rpc, logic(_, _)))

  /**
    * Provides the logic for the given client streaming RPC, also receiving the context.
    *
    * @param rpc the RPC definition.
    * @param logic the logic to handle the request.
    */
  def rpcWithContext[Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    logic: (Streaming[Request], Context) => Unary[Response]
  )(using reqTag: backend.Tag[Request]): ServerService[Unary, Streaming, Context, Rpcs & rpc.type, Backend] =
    ServerService(
      backend,
      serverRpcs :+ server.ServerRpc.ClientStreaming[Unary, Streaming, backend.Tag, Context, Request, Response](rpc, logic(_, _), reqTag)
    )

  /**
    * Provides the logic for the given server streaming RPC, also receiving the context.
    *
    * @param rpc the RPC definition.
    * @param logic the logic to handle the request.
    */
  def rpcWithContext[Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    logic: (Request, Context) => Streaming[Response]
  )(using respTag: backend.Tag[Response]): ServerService[Unary, Streaming, Context, Rpcs & rpc.type, Backend] =
    ServerService(
      backend,
      serverRpcs :+ server.ServerRpc.ServerStreaming[Unary, Streaming, backend.Tag, Context, Request, Response](rpc, logic(_, _), respTag)
    )

  /**
    * Provides the logic for the given bidirectional streaming RPC, also receiving the context.
    *
    * @param rpc the RPC definition.
    * @param logic the logic to handle the request.
    */
  def rpcWithContext[Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    logic: (Streaming[Request], Context) => Streaming[Response]
  )(using reqTag: backend.Tag[Request], respTag: backend.Tag[Response]): ServerService[Unary, Streaming, Context, Rpcs & rpc.type, Backend] =
    ServerService(
      backend,
      serverRpcs :+ server.ServerRpc.BidiStreaming[Unary, Streaming, backend.Tag, Context, Request, Response](rpc, logic(_, _), reqTag, respTag)
    )

  /**
    * Builds the server service definition to be provided to grpc-java.
    * This method can only be called once the logic for all the RPCs has been provided.
    *
    * @param service the service we want to build.
    */
  def build[S](service: Service[S])(using HasAllRpcs[S, Rpcs], HasAllServerRpcs[Rpcs, S]): ServerServiceDefinition = {
    val rpcs = serverRpcs.sortBy(_.name)

    val methodDescriptors: List[MethodDescriptor[?, ?]] =
      rpcs.map(_.toMethodDescriptor(service))

    val fileDescriptor = service.fileDescriptor

    val serviceDescriptor: ServiceDescriptor =
      methodDescriptors
        .foldLeft(
          ServiceDescriptor
            .newBuilder(service.fullyQualifiedName)
            .setSchemaDescriptor(new ProtoFileDescriptorSupplier {
              def getFileDescriptor: FileDescriptor = fileDescriptor
            })
        )((builder, methodDescriptor) => builder.addMethod(methodDescriptor))
        .build()

    (methodDescriptors zip rpcs)
      .foldLeft(ServerServiceDefinition.builder(serviceDescriptor)) { case (builder, (methodDescriptor, rpc)) =>
        builder.addMethod[rpc.Request, rpc.Response](
          methodDescriptor.asInstanceOf[MethodDescriptor[rpc.Request, rpc.Response]],
          backend.handler(rpc)
        )
      }
      .build()
  }
}

object ServerService {

  /**
    * Creates a new server service definition.
    *
    * @param backend the backend to handle the requests.
    */
  private[server] def apply[Unary[_], Streaming[_], Context, Rpcs, Backend <: ServerBackend[Unary, Streaming, Context]](
    backend: Backend,
    serverRpcs: List[ServerRpc[Unary, Streaming, backend.Tag, Context, ?, ?]]
  ): ServerService[Unary, Streaming, Context, Rpcs, Backend] =
    new ServerService(backend, serverRpcs)

  def apply[Unary[_], Streaming[_], Context](
    using backend: ServerBackend[Unary, Streaming, Context]
  ): ServerService[Unary, Streaming, Context, Any, backend.type] =
    ServerService(backend, Nil)
}
