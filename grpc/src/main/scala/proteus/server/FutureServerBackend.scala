package proteus
package server

import scala.concurrent.Future

import io.grpc.{Metadata, ServerCall, ServerCallHandler, Status}

class FutureServerBackend[Context](interceptor: ServerInterceptor[Future, Future, RequestResponseMetadata, Context])
  extends ServerBackend[Future, Future, Context] {
  def handler[Request, Response](
    rpc: ServerRpc[Future, Future, Context, Request, Response]
  ): ServerCallHandler[Request, Response] =
    rpc match {
      case server.ServerRpc.Unary(rpc, logic) =>
        new ServerCallHandler[Request, Response] {
          def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
            call.request(1)
            new ServerCall.Listener[Request] {
              override def onMessage(message: Request): Unit = {
                import scala.concurrent.ExecutionContext.Implicits.global
                val responseMetadata = new Metadata()
                val futureResponse   = interceptor.unary(ctx => logic(message, ctx))(RequestResponseMetadata(headers, responseMetadata))
                futureResponse.onComplete { result =>
                  result match {
                    case scala.util.Success(response) =>
                      call.sendHeaders(new Metadata())
                      call.sendMessage(response)
                      call.close(Status.OK, responseMetadata)
                    case scala.util.Failure(ex)       =>
                      call.close(Status.INTERNAL.withDescription(ex.getMessage).withCause(ex), new Metadata())
                  }
                }
              }
            }
          }
        }
      case _                                  =>
        throw new UnsupportedOperationException("The future backend only supports unary RPCs")
    }
}

object FutureServerBackend {
  def apply: DirectServerBackend[RequestResponseMetadata] =
    new DirectServerBackend(ServerInterceptor.empty)
}
