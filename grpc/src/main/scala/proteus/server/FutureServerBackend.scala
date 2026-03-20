package proteus
package server

import scala.concurrent.Future
import scala.util.control.NonFatal

import io.grpc.{Metadata, ServerCall, ServerCallHandler, Status}

/**
  * A server backend that uses the Future monad to handle RPCs.
  * Streaming is not supported.
  *
  * @param interceptor an interceptor that can run on every request.
  */
class FutureServerBackend[Context](interceptor: ServerContextInterceptor[Future, Future, RequestResponseMetadata, Context])
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
                try {
                  val futureResponse =
                    interceptor.unary(ctx => logic(message, ctx))(using rpc.requestCodec, rpc.responseCodec)(message)(
                      RequestResponseMetadata(headers, responseMetadata)
                    )
                  futureResponse.onComplete {
                    case scala.util.Success(response) =>
                      call.sendHeaders(new Metadata())
                      call.sendMessage(response)
                      call.close(Status.OK, responseMetadata)
                    case scala.util.Failure(ex)       =>
                      ServerBackend.closeCallWithError(call, ex)
                  }
                } catch {
                  case NonFatal(ex) =>
                    ServerBackend.closeCallWithError(call, ex)
                }
              }
            }
          }
        }
      case _                                  =>
        throw new UnsupportedOperationException("The future backend only supports unary RPCs")
    }
}

/**
  * A server backend that uses the Future monad to handle RPCs.
  * Streaming is not supported.
  * This backend doesn't have any interceptors.
  */
object FutureServerBackend extends FutureServerBackend(ServerInterceptor.empty)
