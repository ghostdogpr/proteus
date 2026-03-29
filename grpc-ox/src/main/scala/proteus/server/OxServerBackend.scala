package proteus
package server

import scala.util.control.NonFatal

import io.grpc.{Metadata, ServerCall, ServerCallHandler, Status}
import ox.channels.Channel
import ox.discard
import ox.flow.Flow

/**
  * A server backend that uses direct style with Ox for streaming.
  * Unary RPCs return plain values, streaming RPCs use Ox Flow.
  *
  * @param interceptor an interceptor that can run on every request.
  */
class OxServerBackend[Context](interceptor: ServerContextInterceptor[[A] =>> A, Flow, RequestResponseMetadata, Context])
  extends ServerBackend[[A] =>> A, Flow, Context] {

  private def streamingListener[Request](
    requestChannel: Channel[Request],
    workerThread: Thread,
    readySignal: Channel[Unit]
  ): ServerCall.Listener[Request] =
    new ServerCall.Listener[Request] {
      override def onMessage(message: Request): Unit =
        requestChannel.sendOrClosed(message).discard

      override def onHalfClose(): Unit =
        requestChannel.doneOrClosed().discard

      override def onCancel(): Unit = {
        requestChannel.errorOrClosed(Status.CANCELLED.asException()).discard
        readySignal.doneOrClosed().discard
        workerThread.interrupt()
      }

      override def onReady(): Unit =
        readySignal.sendOrClosed(()).discard
    }

  private def sendWhenReady[Request, Response](call: ServerCall[Request, Response], message: Response, readySignal: Channel[Unit]): Unit = {
    while (!call.isReady)
      readySignal.receiveOrClosed() match {
        case _: ox.channels.ChannelClosed => return // call was cancelled, skip sending
        case _                            => ()
      }
    call.sendMessage(message)
  }

  private def forkHandler[Request, Response](call: ServerCall[Request, Response], body: => Unit): Thread =
    Thread.startVirtualThread { () =>
      try body
      catch {
        // InterruptedException is expected on cancellation — gRPC already closed the call
        case _: InterruptedException => ()
        case NonFatal(ex)            =>
          ServerBackend.closeCallWithError(call, ex)
      }
    }

  def handler[Request, Response](rpc: ServerRpc[[A] =>> A, Flow, Context, Request, Response]): ServerCallHandler[Request, Response] =
    rpc match {
      case ServerRpc.Unary(rpc, logic)           =>
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
                  case NonFatal(ex) =>
                    ServerBackend.closeCallWithError(call, ex)
                }
            }
          }
        }
      case ServerRpc.ClientStreaming(rpc, logic) =>
        new ServerCallHandler[Request, Response] {
          def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
            val requestChannel = Channel.buffered[Request](1)
            val readySignal    = Channel.buffered[Unit](1)
            call.request(1)

            val workerThread = forkHandler(
              call, {
                val requestFlow = Flow.fromSource(requestChannel).tap(_ => call.request(1))

                val responseMetadata = new Metadata()
                val response         =
                  interceptor.clientStreaming[Request, Response](req => ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(
                    requestFlow
                  )(RequestResponseMetadata(headers, responseMetadata))
                call.sendHeaders(new Metadata())
                call.sendMessage(response)
                call.close(Status.OK, responseMetadata)
              }
            )

            streamingListener(requestChannel, workerThread, readySignal)
          }
        }
      case ServerRpc.ServerStreaming(rpc, logic) =>
        new ServerCallHandler[Request, Response] {
          def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
            val readySignal          = Channel.buffered[Unit](1)
            call.request(1)
            var workerThread: Thread = null

            new ServerCall.Listener[Request] {
              override def onMessage(message: Request): Unit =
                workerThread = forkHandler(
                  call, {
                    val responseMetadata = new Metadata()
                    val responseFlow     =
                      interceptor.serverStreaming(ctx => logic(message, ctx))(using rpc.requestCodec, rpc.responseCodec)(message)(
                        RequestResponseMetadata(headers, responseMetadata)
                      )
                    call.sendHeaders(new Metadata())
                    responseFlow.runForeach(resp => sendWhenReady(call, resp, readySignal))
                    call.close(Status.OK, responseMetadata)
                  }
                )

              override def onCancel(): Unit = {
                readySignal.doneOrClosed().discard
                val thread = workerThread
                if (thread != null) thread.interrupt()
              }

              override def onReady(): Unit =
                readySignal.sendOrClosed(()).discard
            }
          }
        }
      case ServerRpc.BidiStreaming(rpc, logic)   =>
        new ServerCallHandler[Request, Response] {
          def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
            val requestChannel = Channel.buffered[Request](1)
            val readySignal    = Channel.buffered[Unit](1)
            call.request(1)

            val workerThread = forkHandler(
              call, {
                val requestFlow = Flow.fromSource(requestChannel).tap(_ => call.request(1))

                val responseMetadata = new Metadata()
                val responseFlow     =
                  interceptor.bidiStreaming[Request, Response](req => ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(
                    requestFlow
                  )(RequestResponseMetadata(headers, responseMetadata))
                call.sendHeaders(new Metadata())
                responseFlow.runForeach(resp => sendWhenReady(call, resp, readySignal))
                call.close(Status.OK, responseMetadata)
              }
            )

            streamingListener(requestChannel, workerThread, readySignal)
          }
        }
    }
}

/**
  * A server backend that uses direct style with Ox for streaming.
  * Unary RPCs return plain values, streaming RPCs use Ox Flow.
  * This backend doesn't have any interceptors.
  */
object OxServerBackend extends OxServerBackend(ServerInterceptor.empty) {

  /**
    * Creates a new Ox server backend with the given interceptor.
    *
    * @param interceptor an interceptor that can run on every request.
    */
  def apply[Context](
    interceptor: ServerContextInterceptor[[A] =>> A, Flow, RequestResponseMetadata, Context]
  ): OxServerBackend[Context] =
    new OxServerBackend(interceptor)
}
