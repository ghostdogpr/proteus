package proteus
package server

import io.grpc.MethodDescriptor
import io.grpc.ServerServiceDefinition

case class ServerService[Unary[_], Streaming[_], Context](service: Service[?], rpcs: List[ServerRpc[Unary, Streaming, Context, ?, ?]])(
  using backend: ServerBackend[Unary, Streaming, Context]
) {
  lazy val definition: ServerServiceDefinition =
    (service.methodDescriptors zip rpcs.sortBy(_.name))
      .foldLeft(ServerServiceDefinition.builder(service.serviceDescriptor)) { case (builder, (methodDescriptor, rpc)) =>
        builder.addMethod[rpc.Request, rpc.Response](
          methodDescriptor.asInstanceOf[MethodDescriptor[rpc.Request, rpc.Response]],
          backend.handler(rpc)
        )
      }
      .build()
}
