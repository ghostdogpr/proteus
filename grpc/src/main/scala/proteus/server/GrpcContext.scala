package proteus
package server

import io.grpc.{Attributes, Metadata, MethodDescriptor, ServerCall}

/**
  * The default context passed to a server handler. Exposes request/response metadata along with
  * call-level information taken from `io.grpc.ServerCall`.
  *
  * @param requestMetadata the metadata sent by the client.
  * @param responseMetadata the metadata that will be sent to the client; the handler may mutate it
  *                         before the response is written.
  * @param authority the authority pseudo-header value used by the client (e.g. host:port), if any.
  * @param methodDescriptor the gRPC method descriptor for the current call.
  * @param attributes transport-level attributes such as remote address or SSL session.
  */
case class GrpcContext(
  requestMetadata: Metadata,
  responseMetadata: Metadata,
  authority: Option[String],
  methodDescriptor: MethodDescriptor[?, ?],
  attributes: Attributes
)

object GrpcContext {

  /**
    * Builds a [[GrpcContext]] from a `ServerCall`, capturing the authority, method descriptor and
    * attributes alongside the supplied request and response metadata.
    */
  def fromCall(call: ServerCall[?, ?], requestMetadata: Metadata, responseMetadata: Metadata): GrpcContext =
    GrpcContext(requestMetadata, responseMetadata, Option(call.getAuthority), call.getMethodDescriptor, call.getAttributes)
}
