package proteus
package server

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

import scala.util.control.NonFatal

import io.grpc.{Metadata, ServerCall, ServerCallHandler, Status}
import ox.{discard, forkCancellable, CancellableFork, InScopeRunner}
import ox.channels.Channel
import ox.flow.Flow

/**
  * A server backend that uses direct style with Ox for streaming.
  * Unary RPCs return plain values, streaming RPCs use Ox Flow.
  *
  * @param interceptor an interceptor that can run on every request.
  * @param runner an InScopeRunner used to start handler threads within a structured concurrency scope.
  * @param prefetchN initial in-flight request window for client-streaming / bidi RPCs; also sizes the request channel buffer.
  */
class OxServerBackend[Context](
  interceptor: ServerContextInterceptor[[A] =>> A, Flow, GrpcContext, Context],
  runner: InScopeRunner,
  prefetchN: Int
) extends ServerBackend[[A] =>> A, Flow, NoTag, Context] {

  private val prefetch: Int = math.max(prefetchN, 1)

  final private class WorkerHandle {
    private val forkRef   = new AtomicReference[CancellableFork[Unit]](null)
    private val cancelled = new AtomicBoolean(false)

    def cancel(): Unit = {
      cancelled.set(true)
      val fork = forkRef.get()
      if (fork != null) fork.cancelNow()
    }

    def publish(fork: CancellableFork[Unit]): Unit = {
      forkRef.set(fork)
      if (cancelled.get()) fork.cancelNow()
    }
  }

  private def streamingListener[Request](
    requestChannel: Channel[Request],
    workerHandle: WorkerHandle,
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
        workerHandle.cancel()
      }

      override def onReady(): Unit =
        readySignal.sendOrClosed(()).discard
    }

  private def sendWhenReady[Request, Response](call: ServerCall[Request, Response], message: Response, readySignal: Channel[Unit]): Unit = {
    while (!call.isReady)
      readySignal.receiveOrClosed() match {
        case _: ox.channels.ChannelClosed => return
        case _                            => ()
      }
    call.sendMessage(message)
  }

  private def sendResponseFlow[Request, Response](
    call: ServerCall[Request, Response],
    responseFlow: Flow[Response],
    readySignal: Channel[Unit]
  ): Unit = {
    var headersSent = false
    responseFlow.runForeach { resp =>
      if (!headersSent) {
        call.sendHeaders(new Metadata())
        headersSent = true
      }
      sendWhenReady(call, resp, readySignal)
    }
  }

  private def forkHandler[Request, Response](
    call: ServerCall[Request, Response],
    workerHandle: WorkerHandle
  )(body: => Unit): Unit =
    runner.async {
      val fork = forkCancellable {
        try body
        catch {
          case _: InterruptedException => ()
          case NonFatal(ex)            => ServerBackend.closeCallWithError(call, ex)
        }
      }
      workerHandle.publish(fork)
    }

  def handler[Request, Response](rpc: ServerRpc[[A] =>> A, Flow, NoTag, Context, Request, Response]): ServerCallHandler[Request, Response] =
    rpc match {
      case ServerRpc.Unary(rpc, logic)              =>
        new ServerCallHandler[Request, Response] {
          def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
            val responseMetadata = new Metadata()
            val ctx              = GrpcContext.fromCall(call, headers, responseMetadata)
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
      case ServerRpc.ClientStreaming(rpc, logic, _) =>
        new ServerCallHandler[Request, Response] {
          def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
            val requestChannel = Channel.buffered[Request](prefetch)
            val readySignal    = Channel.buffered[Unit](1)
            val workerHandle   = new WorkerHandle
            call.request(prefetch)

            forkHandler(call, workerHandle) {
              val requestFlow = Flow.fromSource(requestChannel).tap(_ => call.request(1))

              val responseMetadata = new Metadata()
              val response         =
                interceptor.clientStreaming[Request, Response](req => ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(
                  requestFlow
                )(GrpcContext.fromCall(call, headers, responseMetadata))
              ServerBackend.sendUnaryResponse(call, response)
              call.close(Status.OK, responseMetadata)
            }

            streamingListener(requestChannel, workerHandle, readySignal)
          }
        }
      case ServerRpc.ServerStreaming(rpc, logic, _) =>
        new ServerCallHandler[Request, Response] {
          def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
            val readySignal  = Channel.buffered[Unit](1)
            val workerHandle = new WorkerHandle
            call.request(1)

            new ServerCall.Listener[Request] {
              override def onMessage(message: Request): Unit =
                forkHandler(call, workerHandle) {
                  val responseMetadata = new Metadata()
                  val responseFlow     =
                    interceptor.serverStreaming(ctx => logic(message, ctx))(using rpc.requestCodec, rpc.responseCodec)(message)(
                      GrpcContext.fromCall(call, headers, responseMetadata)
                    )
                  sendResponseFlow(call, responseFlow, readySignal)
                  call.close(Status.OK, responseMetadata)
                }

              override def onCancel(): Unit = {
                readySignal.doneOrClosed().discard
                workerHandle.cancel()
              }

              override def onReady(): Unit =
                readySignal.sendOrClosed(()).discard
            }
          }
        }
      case ServerRpc.BidiStreaming(rpc, logic, _, _) =>
        new ServerCallHandler[Request, Response] {
          def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
            val requestChannel = Channel.buffered[Request](prefetch)
            val readySignal    = Channel.buffered[Unit](1)
            val workerHandle   = new WorkerHandle
            call.request(prefetch)

            forkHandler(call, workerHandle) {
              val requestFlow = Flow.fromSource(requestChannel).tap(_ => call.request(1))

              val responseMetadata = new Metadata()
              val responseFlow     =
                interceptor.bidiStreaming[Request, Response](req => ctx => logic(req, ctx))(using rpc.requestCodec, rpc.responseCodec)(
                  requestFlow
                )(GrpcContext.fromCall(call, headers, responseMetadata))
              sendResponseFlow(call, responseFlow, readySignal)
              call.close(Status.OK, responseMetadata)
            }

            streamingListener(requestChannel, workerHandle, readySignal)
          }
        }
    }
}

object OxServerBackend {

  /**
    * Creates a new Ox server backend with the given InScopeRunner.
    *
    * @param runner an InScopeRunner used to start handler threads within a structured concurrency scope.
    * @param prefetchN initial in-flight request window for client-streaming / bidi RPCs.
    */
  def apply(runner: InScopeRunner, prefetchN: Int = 16): OxServerBackend[GrpcContext] =
    new OxServerBackend(ServerInterceptor.empty, runner, prefetchN)

  /**
    * Creates a new Ox server backend with the given interceptor and InScopeRunner.
    *
    * @param interceptor an interceptor that can run on every request.
    * @param runner an InScopeRunner used to start handler threads within a structured concurrency scope.
    * @param prefetchN initial in-flight request window for client-streaming / bidi RPCs.
    */
  def apply[Context](
    interceptor: ServerContextInterceptor[[A] =>> A, Flow, GrpcContext, Context],
    runner: InScopeRunner,
    prefetchN: Int
  ): OxServerBackend[Context] =
    new OxServerBackend(interceptor, runner, prefetchN)
}
