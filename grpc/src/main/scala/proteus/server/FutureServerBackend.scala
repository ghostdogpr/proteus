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
            val responseMetadata = new Metadata()
            val ctx              = RequestResponseMetadata(headers, responseMetadata)
            call.request(2)
            new UnaryInputListener[Request, Response](call) {
              protected def onRequest(req: Request): Unit = {
                import scala.concurrent.ExecutionContext.Implicits.global
                try {
                  val futureResponse =
                    interceptor.unary(c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(req)(ctx)
                  futureResponse.onComplete {
                    case scala.util.Success(response) =>
                      ServerBackend.sendUnaryResponse(call, response)
                      call.close(Status.OK, responseMetadata)
                    case scala.util.Failure(ex)       =>
                      ServerBackend.closeCallWithError(call, ex, responseMetadata)
                  }
                } catch {
                  case NonFatal(ex) => ServerBackend.closeCallWithError(call, ex, responseMetadata)
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
