package proteus
package server

import java.util.concurrent.atomic.AtomicReference

import io.grpc.{ServerInterceptor as _, *}
import zio.*
import zio.stream.*

import proteus.server.ServerInterceptor

/**
  * A server backend that wraps results in a ZIO effect.
  * Streaming is supported using [[zio.stream.ZStream]].
  *
  * @param interceptor an interceptor that can run on every request.
  * @param runtime the ZIO runtime to use for running ZIO effects.
  */
class ZioServerBackend[R, E, Context](
  interceptor: ServerInterceptor[
    IO[StatusException, *],
    ZIO[R, E, *],
    ZStream[Any, StatusException, *],
    ZStream[R, E, *],
    RequestResponseMetadata,
    Context
  ],
  runtime: Runtime[R]
) extends ServerBackend[ZIO[R, E, *], ZStream[R, E, *], Context] {

  final private class ReadySignal(call: ServerCall[?, ?]) {
    private def mkPromise(): Promise[Nothing, Unit] =
      Unsafe.unsafely(Promise.unsafe.make[Nothing, Unit](FiberId.None))

    private val ref = new AtomicReference[Promise[Nothing, Unit]](mkPromise())

    val await: UIO[Unit] = ZIO.suspendSucceed {
      // Capture promise BEFORE the isReady check so a concurrent signal()
      // (swap + complete) is never lost.
      val current = ref.get()
      if (call.isReady) ZIO.unit
      else current.await
    }

    def signal(): Unit = {
      val old = ref.getAndSet(mkPromise())
      Unsafe.unsafely(runtime.unsafe.run(old.succeed(())).getOrThrowFiberFailure(): Unit)
    }
  }

  private def sendStream[Resp](
    call: ServerCall[?, Resp],
    stream: ZStream[R, StatusException, Resp],
    readySignal: ReadySignal
  ): ZIO[R, StatusException, Unit] = ZIO.suspendSucceed {
    var headersSent = false
    stream.runForeach { resp =>
      val send = ZIO.succeed {
        if (!headersSent) {
          call.sendHeaders(new Metadata())
          headersSent = true
        }
        call.sendMessage(resp)
      }
      if (call.isReady) send else readySignal.await *> send
    }
  }

  private def closeOnExit(call: ServerCall[?, ?], responseMetadata: Metadata)(
    exit: Exit[StatusException, Any]
  ): UIO[Unit] = ZIO.succeed {
    exit match {
      case Exit.Success(_)                                => call.close(Status.OK, responseMetadata)
      case Exit.Failure(cause) if cause.isInterruptedOnly => call.close(Status.CANCELLED, responseMetadata)
      case Exit.Failure(cause)                            =>
        cause.failureOption.orElse(cause.dieOption) match {
          case Some(t) => ServerBackend.closeCallWithError(call, t, responseMetadata)
          case None    => call.close(Status.INTERNAL, responseMetadata)
        }
    }
  }

  final private class CallScope {
    private val ref                              = new AtomicReference[Fiber.Runtime[?, ?]]()
    def attach(fiber: Fiber.Runtime[?, ?]): Unit = ref.set(fiber)
    def cancel(): Unit                           = {
      val f = ref.get()
      if (f != null)
        Unsafe.unsafely(runtime.unsafe.run(f.interruptFork).getOrThrowFiberFailure())
    }
  }

  private def forkScoped(scope: CallScope, effect: ZIO[R, StatusException, Unit]): Unit =
    scope.attach(Unsafe.unsafely(runtime.unsafe.fork(effect)))

  private def sendUnaryResponse[Response](call: ServerCall[?, Response], response: Response): UIO[Unit] =
    ZIO.succeed(ServerBackend.sendUnaryResponse(call, response))

  def handler[Request, Response](
    rpc: ServerRpc[ZIO[R, E, *], ZStream[R, E, *], Context, Request, Response]
  ): ServerCallHandler[Request, Response] =
    rpc match {
      case ServerRpc.Unary(rpc, logic)           => unaryHandler(rpc, logic)
      case ServerRpc.ClientStreaming(rpc, logic) => clientStreamingHandler(rpc, logic)
      case ServerRpc.ServerStreaming(rpc, logic) => serverStreamingHandler(rpc, logic)
      case ServerRpc.BidiStreaming(rpc, logic)   => bidiStreamingHandler(rpc, logic)
    }

  private def unaryHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (Request, Context) => ZIO[R, E, Response]
  ): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = RequestResponseMetadata(headers, responseMetadata)
        call.request(2)

        new UnaryInputListener[Request, Response](call) {
          protected def onRequest(req: Request): Unit = {
            val effect: IO[StatusException, Unit] =
              interceptor
                .unary(c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(req)(ctx)
                .flatMap(sendUnaryResponse(call, _))
            forkScoped(scope, effect.onExit(closeOnExit(call, responseMetadata)))
          }

          override protected def onCallCancelled(): Unit = scope.cancel()
        }
      }
    }

  private def serverStreamingHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (Request, Context) => ZStream[R, E, Response]
  ): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = RequestResponseMetadata(headers, responseMetadata)
        val readySignal      = new ReadySignal(call)
        call.request(2)

        new UnaryInputListener[Request, Response](call) {
          protected def onRequest(req: Request): Unit = {
            val responseStream = interceptor.serverStreaming(c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(req)(ctx)
            val effect         = sendStream(call, responseStream, readySignal)
            forkScoped(scope, effect.onExit(closeOnExit(call, responseMetadata)))
          }

          override protected def onCallCancelled(): Unit = scope.cancel()
          override protected def onCallReady(): Unit     = readySignal.signal()
        }
      }
    }

  private def unsafeOffer[A](queue: Queue[A], a: A): Unit =
    Unsafe.unsafely(runtime.unsafe.run(queue.offer(a)).getOrThrowFiberFailure(): Unit)

  private def streamingInputListener[Request](
    call: ServerCall[Request, ?],
    queue: Queue[Option[Request]],
    scope: CallScope,
    onReadyCallback: () => Unit
  ): ServerCall.Listener[Request] =
    new ServerCall.Listener[Request] {
      override def onMessage(message: Request): Unit = {
        call.request(1)
        unsafeOffer(queue, Some(message))
      }

      override def onHalfClose(): Unit =
        unsafeOffer(queue, None)

      override def onCancel(): Unit = scope.cancel()

      override def onReady(): Unit = onReadyCallback()
    }

  private def clientStreamingHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (ZStream[R, E, Request], Context) => ZIO[R, E, Response]
  ): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val queue            = Unsafe.unsafely(Queue.unsafe.unbounded[Option[Request]](FiberId.None))
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = RequestResponseMetadata(headers, responseMetadata)
        call.request(1)

        val requestStream  = ZStream.fromQueue(queue).collectWhileSome
        val responseEffect =
          interceptor.clientStreaming[Request, Response](req => c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(requestStream)(ctx)
        val effect         = responseEffect.flatMap(sendUnaryResponse(call, _))
        forkScoped(scope, effect.onExit(closeOnExit(call, responseMetadata)))

        streamingInputListener(call, queue, scope, () => ())
      }
    }

  private def bidiStreamingHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (ZStream[R, E, Request], Context) => ZStream[R, E, Response]
  ): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val queue            = Unsafe.unsafely(Queue.unsafe.unbounded[Option[Request]](FiberId.None))
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = RequestResponseMetadata(headers, responseMetadata)
        val readySignal      = new ReadySignal(call)
        call.request(1)

        val requestStream  = ZStream.fromQueue(queue).collectWhileSome
        val responseStream =
          interceptor.bidiStreaming[Request, Response](req => c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(requestStream)(ctx)
        val effect         = sendStream(call, responseStream, readySignal)
        forkScoped(scope, effect.onExit(closeOnExit(call, responseMetadata)))

        streamingInputListener(call, queue, scope, () => readySignal.signal())
      }
    }
}

object ZioServerBackend extends ZioServerBackend(ServerInterceptor.empty, Runtime.default) {

  /**
    * A layer that provides a [[ZioServerBackend]] with the current ZIO runtime.
    */
  val layer: ULayer[ZioServerBackend[Any, StatusException, RequestResponseMetadata]] =
    ZLayer(ZIO.runtime[Any].map(new ZioServerBackend(ServerInterceptor.empty, _)))

  def apply(): ZioServerBackend[Any, StatusException, RequestResponseMetadata] =
    new ZioServerBackend(ServerInterceptor.empty, Runtime.default)

  def apply[R, E, Context](
    interceptor: ServerInterceptor[
      IO[StatusException, *],
      ZIO[R, E, *],
      ZStream[Any, StatusException, *],
      ZStream[R, E, *],
      RequestResponseMetadata,
      Context
    ],
    runtime: Runtime[R]
  ): ZioServerBackend[R, E, Context] =
    new ZioServerBackend(interceptor, runtime)
}
