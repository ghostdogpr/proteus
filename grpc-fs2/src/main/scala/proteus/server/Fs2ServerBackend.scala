package proteus
package server

import java.util.concurrent.atomic.AtomicReference

import cats.effect.kernel.{Async, Deferred, Outcome}
import cats.effect.std.{Dispatcher, Queue}
import cats.syntax.all.*
import fs2.Stream
import io.grpc.{ServerInterceptor as _, *}

import proteus.server.ServerInterceptor

/**
  * A server backend that wraps results in an abstract `F[_]` monad backed by Cats Effect typeclasses.
  * Streaming is supported using [[fs2.Stream]].
  *
  * @param interceptor an interceptor that can run on every request.
  * @param dispatcher a Cats Effect `Dispatcher` used to bridge gRPC's synchronous callbacks into `F`.
  * @param prefetchN initial in-flight request window for client-streaming / bidi RPCs; the window is refilled one message at a time as the handler consumes from the request stream.
  */
class Fs2ServerBackend[F[_]: Async, G[_], Context](
  interceptor: ServerInterceptor[F, G, Stream[F, *], Stream[G, *], GrpcContext, Context],
  dispatcher: Dispatcher[F],
  prefetchN: Int
) extends ServerBackend[G, Stream[G, *], NoTag, Context] {

  private val F        = Async[F]
  private val prefetch = math.max(prefetchN, 1)

  final private class ReadySignal(call: ServerCall[?, ?]) {
    private def mkDeferred(): Deferred[F, Unit] = Deferred.unsafe[F, Unit]

    private val ref = new AtomicReference[Deferred[F, Unit]](mkDeferred())

    val await: F[Unit] = F.defer {
      // Capture deferred BEFORE the isReady check so a concurrent signal() is never lost.
      val current = ref.get()
      if (call.isReady) F.unit
      else current.get
    }

    def signal(): Unit = {
      val old = ref.getAndSet(mkDeferred())
      dispatcher.unsafeRunAndForget(old.complete(()))
    }
  }

  private def sendStream[Resp](
    call: ServerCall[?, Resp],
    stream: Stream[F, Resp],
    readySignal: ReadySignal
  ): F[Unit] = F.defer {
    var headersSent = false
    stream
      .evalMap { resp =>
        val send = F.delay {
          if (!headersSent) {
            call.sendHeaders(new Metadata())
            headersSent = true
          }
          call.sendMessage(resp)
        }
        F.defer(if (call.isReady) send else readySignal.await *> send)
      }
      .compile
      .drain
  }

  private def closeOnExit(call: ServerCall[?, ?], responseMetadata: Metadata)(
    outcome: Outcome[F, Throwable, Unit]
  ): F[Unit] = F.delay {
    outcome match {
      case Outcome.Succeeded(_) => call.close(Status.OK, responseMetadata)
      case Outcome.Canceled()   => call.close(Status.CANCELLED, responseMetadata)
      case Outcome.Errored(e)   => ServerBackend.closeCallWithError(call, e, responseMetadata)
    }
  }

  final private class CallScope {
    private val ref                                               = new AtomicReference[() => scala.concurrent.Future[Unit]]()
    def attach(cancel: () => scala.concurrent.Future[Unit]): Unit = ref.set(cancel)
    def cancel(): Unit                                            = {
      val c = ref.get()
      if (c != null) { c(); () }
    }
  }

  private def forkScoped(scope: CallScope, effect: F[Unit]): Unit =
    scope.attach(dispatcher.unsafeRunCancelable(effect))

  private def sendUnaryResponse[Response](call: ServerCall[?, Response], response: Response): F[Unit] =
    F.delay(ServerBackend.sendUnaryResponse(call, response))

  def handler[Request, Response](
    rpc: ServerRpc[G, Stream[G, *], NoTag, Context, Request, Response]
  ): ServerCallHandler[Request, Response] =
    rpc match {
      case ServerRpc.Unary(rpc, logic)               => unaryHandler(rpc, logic)
      case ServerRpc.ClientStreaming(rpc, logic, _)  => clientStreamingHandler(rpc, logic)
      case ServerRpc.ServerStreaming(rpc, logic, _)  => serverStreamingHandler(rpc, logic)
      case ServerRpc.BidiStreaming(rpc, logic, _, _) => bidiStreamingHandler(rpc, logic)
    }

  private def unaryHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (Request, Context) => G[Response]
  ): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = GrpcContext.fromCall(call, headers, responseMetadata)
        call.request(2)

        new UnaryInputListener[Request, Response](call) {
          protected def onRequest(req: Request): Unit = {
            val effect: F[Unit] =
              interceptor
                .unary(c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(req)(ctx)
                .flatMap(sendUnaryResponse(call, _))
            forkScoped(scope, F.guaranteeCase(effect)(closeOnExit(call, responseMetadata)))
          }

          override protected def onCallCancelled(): Unit = scope.cancel()
        }
      }
    }

  private def serverStreamingHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (Request, Context) => Stream[G, Response]
  ): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = GrpcContext.fromCall(call, headers, responseMetadata)
        val readySignal      = new ReadySignal(call)
        call.request(2)

        new UnaryInputListener[Request, Response](call) {
          protected def onRequest(req: Request): Unit = {
            val responseStream =
              interceptor.serverStreaming(c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(req)(ctx)
            val effect         = sendStream(call, responseStream, readySignal)
            forkScoped(scope, F.guaranteeCase(effect)(closeOnExit(call, responseMetadata)))
          }

          override protected def onCallCancelled(): Unit = scope.cancel()
          override protected def onCallReady(): Unit     = readySignal.signal()
        }
      }
    }

  private def unsafeOffer[A](queue: Queue[F, A], a: A): Unit =
    dispatcher.unsafeRunSync(queue.offer(a))

  private def streamingInputListener[Request](
    queue: Queue[F, Option[Request]],
    scope: CallScope,
    onReadyCallback: () => Unit
  ): ServerCall.Listener[Request] =
    new ServerCall.Listener[Request] {
      override def onMessage(message: Request): Unit =
        unsafeOffer(queue, Some(message))

      override def onHalfClose(): Unit =
        unsafeOffer(queue, None)

      override def onCancel(): Unit = scope.cancel()

      override def onReady(): Unit = onReadyCallback()
    }

  private def streamingRequestStream[Request](
    call: ServerCall[Request, ?],
    queue: Queue[F, Option[Request]]
  ): Stream[F, Request] =
    Stream
      .fromQueueNoneTerminated(queue, prefetch)
      .chunks
      .evalTap(chunk => F.delay(call.request(chunk.size)))
      .unchunks

  private def clientStreamingHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (Stream[G, Request], Context) => G[Response]
  ): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val queue            = dispatcher.unsafeRunSync(Queue.unbounded[F, Option[Request]])
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = GrpcContext.fromCall(call, headers, responseMetadata)
        call.request(prefetch)

        val requestStream  = streamingRequestStream(call, queue)
        val responseEffect = interceptor
          .clientStreaming[Request, Response](req => c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(requestStream)(ctx)
        val effect         = responseEffect.flatMap(sendUnaryResponse(call, _))
        forkScoped(scope, F.guaranteeCase(effect)(closeOnExit(call, responseMetadata)))

        streamingInputListener(queue, scope, () => ())
      }
    }

  private def bidiStreamingHandler[Request, Response](
    rpc: Rpc[Request, Response],
    logic: (Stream[G, Request], Context) => Stream[G, Response]
  ): ServerCallHandler[Request, Response] =
    new ServerCallHandler[Request, Response] {
      def startCall(call: ServerCall[Request, Response], headers: Metadata): ServerCall.Listener[Request] = {
        val queue            = dispatcher.unsafeRunSync(Queue.unbounded[F, Option[Request]])
        val scope            = new CallScope
        val responseMetadata = new Metadata()
        val ctx              = GrpcContext.fromCall(call, headers, responseMetadata)
        val readySignal      = new ReadySignal(call)
        call.request(prefetch)

        val requestStream  = streamingRequestStream(call, queue)
        val responseStream = interceptor
          .bidiStreaming[Request, Response](req => c => logic(req, c))(using rpc.requestCodec, rpc.responseCodec)(requestStream)(ctx)
        val effect         = sendStream(call, responseStream, readySignal)
        forkScoped(scope, F.guaranteeCase(effect)(closeOnExit(call, responseMetadata)))

        streamingInputListener(queue, scope, () => readySignal.signal())
      }
    }
}

object Fs2ServerBackend {

  /**
    * Creates a new fs2 server backend.
    *
    * @param dispatcher a Cats Effect dispatcher.
    * @param prefetchN initial in-flight request window for client-streaming / bidi RPCs.
    */
  def apply[F[_]: Async](
    dispatcher: Dispatcher[F],
    prefetchN: Int = 16
  ): Fs2ServerBackend[F, F, GrpcContext] =
    apply(ServerInterceptor.empty, dispatcher, prefetchN)

  /**
    * Creates a new fs2 server backend with the given context interceptor.
    *
    * @param interceptor an interceptor that can run on every request.
    * @param dispatcher a Cats Effect dispatcher.
    * @param prefetchN initial in-flight request window for client-streaming / bidi RPCs.
    */
  def apply[F[_]: Async, Context](
    interceptor: ServerContextInterceptor[F, Stream[F, *], GrpcContext, Context],
    dispatcher: Dispatcher[F],
    prefetchN: Int
  ): Fs2ServerBackend[F, F, Context] =
    new Fs2ServerBackend(interceptor, dispatcher, prefetchN)

  /**
    * Creates a new fs2 server backend with the given interceptor, allowing the user-handler
    * effect `G` to differ from the transport effect `F`.
    *
    * @param interceptor an interceptor that can run on every request.
    * @param dispatcher a Cats Effect dispatcher.
    * @param prefetchN initial in-flight request window for client-streaming / bidi RPCs.
    */
  def apply[F[_]: Async, G[_], Context](
    interceptor: ServerInterceptor[F, G, Stream[F, *], Stream[G, *], GrpcContext, Context],
    dispatcher: Dispatcher[F],
    prefetchN: Int
  ): Fs2ServerBackend[F, G, Context] =
    new Fs2ServerBackend(interceptor, dispatcher, prefetchN)
}
