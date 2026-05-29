package proteus
package server

import java.util.concurrent.atomic.AtomicReference

import io.grpc.{Metadata, ServerCall, ServerCallHandler, Status, StatusException}
import kyo.*
import kyo.AllowUnsafe.embrace.danger
import kyo.Result.{Failure, Panic, Success}

/**
  * A server backend that uses Kyo. Unary RPCs return `A < (Async & Abort[StatusException])`, streaming RPCs use [[kyo.Stream]].
  * Handlers run on Kyo's own scheduler, so no runtime needs to be supplied.
  *
  * @param interceptor an interceptor that can run on every request.
  * @param prefetchN initial in-flight request window for client-streaming / bidi RPCs; also sizes the request channel buffer.
  */
class KyoServerBackend[S, Context](
  interceptor: ServerInterceptor[
    [A] =>> A < (Async & Abort[StatusException]),
    [A] =>> A < S,
    [A] =>> Stream[A, Async & Abort[StatusException]],
    [A] =>> Stream[A, S],
    GrpcContext,
    Context
  ],
  prefetchN: Int
) extends ServerBackend[[A] =>> A < S, [A] =>> Stream[A, S], Tag, Context] {

  private val prefetch: Int = math.max(prefetchN, 1)

  final private class CallScope {
    private val ref                                                     = new AtomicReference[Maybe[Fiber.Unsafe[Unit, Abort[StatusException]]]](Absent)
    def attach(fiber: Fiber.Unsafe[Unit, Abort[StatusException]]): Unit = ref.set(Present(fiber))
    def cancel(): Unit                                                  = ref.get().foreach { f =>
      val _ = f.interrupt()
    }
  }

  final private class ReadySignal(call: ServerCall[?, ?]) {
    private val readyEvents = Channel.Unsafe.init[Unit](1)

    val await: Unit < Async =
      Loop.whileTrue(Sync.defer(!call.isReady)) {
        Abort.run[Closed](readyEvents.safe.take).unit
      }

    def signal(): Unit = { val _ = readyEvents.offer(()) }
  }

  private def forkScoped(scope: CallScope, program: Unit < (Async & Abort[StatusException]))(
    onComplete: Result[StatusException, Unit] => Unit
  ): Unit = {
    val fiber = Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(program))
    fiber.unsafe.onComplete(r => onComplete(r.map(_.eval)))
    scope.attach(fiber.unsafe)
  }

  private def closeOnExit(call: ServerCall[?, ?], responseMetadata: Metadata)(result: Result[StatusException, Unit]): Unit =
    result match {
      case Success(_)            => call.close(Status.OK, responseMetadata)
      case Panic(_: Interrupted) => call.close(Status.CANCELLED, responseMetadata)
      case Failure(status)       => ServerBackend.closeCallWithError(call, status, responseMetadata)
      case Panic(ex)             => ServerBackend.closeCallWithError(call, ex, responseMetadata)
    }

  private def sendStream[Response](
    call: ServerCall[?, Response],
    responseStream: Stream[Response, Async & Abort[StatusException]],
    readySignal: ReadySignal
  )(using Tag[Response]): Unit < (Async & Abort[StatusException]) =
    Sync.defer {
      var headersSent = false
      responseStream.foreach { resp =>
        readySignal.await
          .andThen(Sync.defer {
            if (!headersSent) {
              call.sendHeaders(new Metadata())
              headersSent = true
            }
            call.sendMessage(resp)
          })
      }
    }

  private def streamingInputListener[Request](
    channel: Channel.Unsafe[Request],
    scope: CallScope,
    onReadyCallback: () => Unit
  ): ServerCall.Listener[Request] =
    new ServerCall.Listener[Request] {
      override def onMessage(message: Request): Unit = { val _ = channel.offer(message) }
      override def onHalfClose(): Unit               = { val _ = channel.closeAwaitEmpty() }
      override def onCancel(): Unit                  = {
        val _ = channel.close()
        scope.cancel()
      }
      override def onReady(): Unit                   = onReadyCallback()
    }

  private def requestStream[Request](call: ServerCall[Request, ?], channel: Channel[Request])(
    using Tag[Request]
  ): Stream[Request, Async & Abort[StatusException]] =
    channel.streamUntilClosed(1).tap(_ => Sync.defer(call.request(1)))

  def handler[Request, Response](
    rpc: ServerRpc[[A] =>> A < S, [A] =>> Stream[A, S], Tag, Context, Request, Response]
  ): ServerCallHandler[Request, Response] =
    rpc match {
      case ServerRpc.Unary(rpc, logic)                          => unaryHandler(rpc, logic)
      case ServerRpc.ClientStreaming(rpc, logic, reqTag)        => clientStreamingHandler(rpc, logic)(using reqTag)
      case ServerRpc.ServerStreaming(rpc, logic, respTag)       => serverStreamingHandler(rpc, logic)(using respTag)
      case ServerRpc.BidiStreaming(rpc, logic, reqTag, respTag) => bidiStreamingHandler(rpc, logic)(using reqTag, respTag)
    }

  private def unaryHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (Request, Context) => Response < S
  ): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = GrpcContext.fromCall(call, headers, responseMetadata)
        call.request(2)

        new UnaryInputListener[Request, Response](call) {
          protected def onRequest(req: Request): Unit = {
            val program =
              interceptor
                .unary(c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(req)(ctx)
                .map(resp => Sync.defer(ServerBackend.sendUnaryResponse(call, resp)))
            forkScoped(scope, program)(closeOnExit(call, responseMetadata))
          }

          override protected def onCallCancelled(): Unit = scope.cancel()
        }
      }
    }

  private def serverStreamingHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (Request, Context) => Stream[Response, S]
  )(using Tag[Response]): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = GrpcContext.fromCall(call, headers, responseMetadata)
        val readySignal      = new ReadySignal(call)
        call.request(2)

        new UnaryInputListener[Request, Response](call) {
          protected def onRequest(req: Request): Unit = {
            val responseStream = interceptor.serverStreaming(c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(req)(ctx)
            val program        = sendStream(call, responseStream, readySignal)
            forkScoped(scope, program)(closeOnExit(call, responseMetadata))
          }

          override protected def onCallCancelled(): Unit = scope.cancel()
          override protected def onCallReady(): Unit     = readySignal.signal()
        }
      }
    }

  private def clientStreamingHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (Stream[Request, S], Context) => Response < S
  )(using Tag[Request]): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = GrpcContext.fromCall(call, headers, responseMetadata)
        val channel          = Channel.Unsafe.init[Request](prefetch)
        call.request(prefetch)

        val program =
          interceptor
            .clientStreaming[Request, Response](req => c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(
              requestStream(call, channel.safe)
            )(ctx)
            .map(resp => Sync.defer(ServerBackend.sendUnaryResponse(call, resp)))
        forkScoped(scope, program)(closeOnExit(call, responseMetadata))

        streamingInputListener(channel, scope, () => ())
      }
    }

  private def bidiStreamingHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (Stream[Request, S], Context) => Stream[Response, S]
  )(using Tag[Request], Tag[Response]): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = GrpcContext.fromCall(call, headers, responseMetadata)
        val readySignal      = new ReadySignal(call)
        val channel          = Channel.Unsafe.init[Request](prefetch)
        call.request(prefetch)

        val responseStream =
          interceptor.bidiStreaming[Request, Response](req => c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(
            requestStream(call, channel.safe)
          )(ctx)
        val program        = sendStream(call, responseStream, readySignal)
        forkScoped(scope, program)(closeOnExit(call, responseMetadata))

        streamingInputListener(channel, scope, () => readySignal.signal())
      }
    }
}

object KyoServerBackend {

  /**
    * Creates a new Kyo server backend with no interceptor.
    *
    * @param prefetchN initial in-flight request window for client-streaming / bidi RPCs.
    */
  def apply(prefetchN: Int = 16): KyoServerBackend[Async & Abort[StatusException], GrpcContext] =
    new KyoServerBackend(
      ServerInterceptor.empty[[A] =>> A < (Async & Abort[StatusException]), [A] =>> Stream[A, Async & Abort[StatusException]], GrpcContext],
      prefetchN
    )

  /**
    * Creates a new Kyo server backend with the given interceptor.
    *
    * @param interceptor an interceptor that can run on every request.
    * @param prefetchN initial in-flight request window for client-streaming / bidi RPCs.
    */
  def apply[S, Context](
    interceptor: ServerInterceptor[
      [A] =>> A < (Async & Abort[StatusException]),
      [A] =>> A < S,
      [A] =>> Stream[A, Async & Abort[StatusException]],
      [A] =>> Stream[A, S],
      GrpcContext,
      Context
    ],
    prefetchN: Int
  ): KyoServerBackend[S, Context] =
    new KyoServerBackend(interceptor, prefetchN)
}
