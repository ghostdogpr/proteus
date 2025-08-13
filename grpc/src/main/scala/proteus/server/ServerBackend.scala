package proteus
package server

import scala.concurrent.Future

import io.grpc.{Metadata, ServerCall, ServerCallHandler, Status}

trait ServerBackend[Unary[_], Streaming[_], Context] { self =>
  def handler[Request, Response](rpc: ServerRpc[Unary, Streaming, Context, Request, Response]): ServerCallHandler[Request, Response]
}

object ServerBackend {

  case class RequestResponseMetadata(requestMetadata: Metadata, responseMetadata: Metadata)

  val direct: ServerBackend[[A] =>> A, [A] =>> A, RequestResponseMetadata] =
    directWith(server.ServerInterceptor.empty)

  def directWith[Context](
    interceptor: ServerInterceptor[[A] =>> A, [A] =>> A, RequestResponseMetadata, Context]
  ): ServerBackend[[A] =>> A, [A] =>> A, Context] =
    new ServerBackend[[A] =>> A, [A] =>> A, Context] {
      def handler[Request, Response](rpc: ServerRpc[[A] =>> A, [A] =>> A, Context, Request, Response]): ServerCallHandler[Request, Response] =
        rpc match {
          case server.ServerRpc.Unary(_, logic) =>
            new ServerCallHandler[Request, Response] {
              def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
                call.request(1)
                new ServerCall.Listener[Request] {
                  override def onMessage(message: Request): Unit =
                    try {
                      val responseMetadata = new Metadata()
                      val response         = interceptor.unary(ctx => logic(message, ctx))(RequestResponseMetadata(headers, responseMetadata))
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
          case _                                =>
            throw new UnsupportedOperationException("The direct backend only supports unary RPCs")
        }
    }

  val future: ServerBackend[Future, Future, RequestResponseMetadata] =
    futureWith(server.ServerInterceptor.empty)

  def futureWith[Context](
    interceptor: ServerInterceptor[Future, Future, RequestResponseMetadata, Context]
  ): ServerBackend[Future, Future, Context] =
    new ServerBackend[Future, Future, Context] {
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
}
