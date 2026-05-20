package proteus
package server

import scala.util.control.NonFatal

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
            val responseMetadata = new Metadata()
            val ctx              = RequestResponseMetadata(headers, responseMetadata)
            call.request(2)
            new UnaryInputListener[Request, Response](call) {
              protected def onRequest(req: Request): Unit =
                try {
                  val response =
                    interceptor.unary(c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(req)(ctx)
                  ServerBackend.sendUnaryResponse(call, response)
                  call.close(Status.OK, responseMetadata)
                } catch {
                  case NonFatal(ex) => ServerBackend.closeCallWithError(call, ex, responseMetadata)
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
