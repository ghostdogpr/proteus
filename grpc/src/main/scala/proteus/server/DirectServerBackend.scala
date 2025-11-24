package proteus
package server

import io.grpc.{Metadata, ServerCall, ServerCallHandler, Status}

/**
  * A server backend that uses direct style to handle RPCs (no wrapper monad is used).
  * Streaming is not supported.
  *
  * @param interceptor an interceptor that can run on every request.
  */
class DirectServerBackend[Context](interceptor: ServerContextInterceptor[[A] =>> A, [A] =>> A, RequestResponseMetadata, Context])
  extends ServerBackend[[A] =>> A, [A] =>> A, Context] {
  def handler[Request, Response](rpc: ServerRpc[[A] =>> A, [A] =>> A, Context, Request, Response]): ServerCallHandler[Request, Response] =
    rpc match {
      case server.ServerRpc.Unary(rpc, logic) =>
        new ServerCallHandler[Request, Response] {
          def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
            call.request(1)
            new ServerCall.Listener[Request] {
              override def onMessage(message: Request): Unit =
                try {
                  val responseMetadata = new Metadata()
                  val response         =
                    interceptor.unary(ctx => logic(message, ctx))(using rpc.requestCodec, rpc.responseCodec)(message)(
                      RequestResponseMetadata(headers, responseMetadata)
                    )
                  call.sendHeaders(new Metadata())
                  call.sendMessage(response)
                  call.close(Status.OK, responseMetadata)
                } catch {
                  case ex: Exception =>
                    call.close(Status.INTERNAL.withDescription(ex.getMessage).withCause(ex), new Metadata())
                }
            }
          }
        }
      case _                                  =>
        throw new UnsupportedOperationException("The direct backend only supports unary RPCs")
    }
}

/**
  * A server backend that uses direct style to handle RPCs (no wrapper monad is used).
  * Streaming is not supported.
  * This backend doesn't have any interceptors.
  */
object DirectServerBackend extends DirectServerBackend(ServerInterceptor.empty)
