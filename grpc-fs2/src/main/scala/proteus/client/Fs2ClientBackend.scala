package proteus
package client

import java.util.concurrent.atomic.AtomicReference

import cats.effect.kernel.{Async, Deferred}
import cats.effect.std.{Dispatcher, Queue}
import cats.syntax.all.*
import fs2.Stream
import io.grpc.*

/**
  * A client backend that wraps results in an abstract `F[_]` monad backed by Cats Effect typeclasses.
  * Streaming is supported using [[fs2.Stream]].
  */
class Fs2ClientBackend[F[_]: Async](channel: Channel, dispatcher: Dispatcher[F]) extends ClientBackend[F, Stream[F, *]] {

  private val F = Async[F]

  final private class UnaryListener[Response] extends ClientCall.Listener[Response] {
    private val headersRef                                                             = new AtomicReference[Metadata](null)
    private val messageRef                                                             = new AtomicReference[Response]()
    val deferred: Deferred[F, Either[StatusException, (Response, Metadata, Metadata)]] =
      Deferred.unsafe[F, Either[StatusException, (Response, Metadata, Metadata)]]

    override def onHeaders(headers: Metadata): Unit =
      headersRef.set(headers)

    override def onMessage(message: Response): Unit =
      messageRef.set(message)

    override def onClose(status: Status, trailers: Metadata): Unit = {
      val result: Either[StatusException, (Response, Metadata, Metadata)] =
        if (status.isOk) {
          val msg = messageRef.get()
          if (msg == null) Left(Status.INTERNAL.withDescription("No data received").asException())
          else Right((msg, headersRef.get(), trailers)) // headers may be null; consumers handle it
        } else Left(new StatusException(status, trailers))
      dispatcher.unsafeRunAndForget(deferred.complete(result))
    }
  }

  private class StreamingListener[Response](queue: Queue[F, Either[Option[StatusException], Response]]) extends ClientCall.Listener[Response] {
    val trailersRef = new AtomicReference[Metadata](new Metadata())

    protected def offer(a: Either[Option[StatusException], Response]): Unit =
      dispatcher.unsafeRunSync(queue.offer(a))

    override def onHeaders(headers: Metadata): Unit = ()

    override def onMessage(message: Response): Unit = offer(Right(message))

    override def onClose(status: Status, trailers: Metadata): Unit = {
      trailersRef.set(trailers)
      if (status.isOk) offer(Left(None))
      else offer(Left(Some(new StatusException(status, trailers))))
    }
  }

  final private class ClientReadySignal(call: ClientCall[?, ?]) {
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

  final private class BidiListener[Response](
    queue: Queue[F, Either[Option[StatusException], Response]],
    val readySignal: ClientReadySignal
  ) extends StreamingListener[Response](queue) {
    override def onReady(): Unit = readySignal.signal()
  }

  private def fromDeferred[A](d: Deferred[F, Either[StatusException, A]]): F[A] =
    d.get.flatMap {
      case Right(a) => F.pure(a)
      case Left(e)  => F.raiseError(e)
    }

  private def unaryRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    request: Request
  ): F[(Response, Metadata, Metadata)] = F.defer {
    val call     = channel.newCall(descriptor, options)
    val listener = new UnaryListener[Response]
    F.delay {
      call.start(listener, headers)
      call.request(2)
      call.sendMessage(request)
      call.halfClose()
    }.attempt
      .flatMap {
        case Left(e)  =>
          F.delay(call.cancel("Failed to send unary request", e)) *> F.raiseError(ClientBackend.toStatusException(e))
        case Right(_) =>
          F.onCancel(fromDeferred(listener.deferred), F.delay(call.cancel("Interrupted", null)))
      }
  }

  private def streamFromQueue[Response](
    call: ClientCall[?, Response],
    queue: Queue[F, Either[Option[StatusException], Response]]
  ): Stream[F, Response] =
    Stream
      .repeatEval(queue.take)
      .evalMap {
        case Right(v)      => F.delay { call.request(1); Some(v): Option[Response] }
        case Left(None)    => F.pure(None: Option[Response])
        case Left(Some(e)) => F.raiseError[Option[Response]](e)
      }
      .unNoneTerminate

  private def serverStreamingRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    request: Request
  ): Stream[F, Response] =
    Stream
      .eval(F.delay {
        val call  = channel.newCall(descriptor, options)
        val queue = dispatcher.unsafeRunSync(Queue.unbounded[F, Either[Option[StatusException], Response]])
        val l     = new StreamingListener[Response](queue)
        call.start(l, headers)
        call.request(1)
        call.sendMessage(request)
        call.halfClose()
        (call, queue)
      })
      .flatMap { case (call, queue) =>
        streamFromQueue(call, queue).onFinalize(F.delay(call.cancel("Stream ended", null)))
      }

  private def clientStreamingRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    requestStream: Stream[F, Request]
  ): F[(Response, Metadata, Metadata)] = F.defer {
    val call         = channel.newCall(descriptor, options)
    val listener     = new UnaryListener[Response]
    val readySignal  = new ClientReadySignal(call)
    val bidiListener = new ClientCall.Listener[Response] {
      override def onHeaders(h: Metadata): Unit          = listener.onHeaders(h)
      override def onMessage(m: Response): Unit          = listener.onMessage(m)
      override def onClose(s: Status, t: Metadata): Unit = listener.onClose(s, t)
      override def onReady(): Unit                       = readySignal.signal()
    }
    call.start(bidiListener, headers)
    call.request(2)

    val sendAll: F[Unit] =
      requestStream
        .evalMap { req =>
          F.defer {
            if (call.isReady) F.delay(call.sendMessage(req))
            else readySignal.await *> F.delay(call.sendMessage(req))
          }
        }
        .compile
        .drain *> F.delay(call.halfClose())

    // Sender surfaces failures via the response deferred; on success the server completes it via onClose.
    val send: F[Unit] = sendAll.handleErrorWith { e =>
      val statusEx = e match {
        case se: StatusException => se
        case t                   => ClientBackend.toStatusException(t)
      }
      F.delay(call.cancel("Error sending requests", e)) <* listener.deferred.complete(Left(statusEx))
    }

    // Server may respond and close before the request stream is fully drained — cancel
    // the sender as soon as the response arrives, to avoid pushing into a closed call.
    val raced: F[(Response, Metadata, Metadata)] =
      F.uncancelable { poll =>
        F.racePair(send, poll(fromDeferred(listener.deferred))).flatMap {
          case Left((_, respFiber))            => respFiber.joinWith(F.never)
          case Right((sendFiber, respOutcome)) => sendFiber.cancel *> respOutcome.embedNever
        }
      }
    F.onCancel(raced, F.delay(call.cancel("Interrupted", null)))
  }

  private def bidiRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    requestStream: Stream[F, Request]
  ): Stream[F, Response] =
    Stream
      .eval(F.delay {
        val call        = channel.newCall(descriptor, options)
        val queue       = dispatcher.unsafeRunSync(Queue.unbounded[F, Either[Option[StatusException], Response]])
        val readySignal = new ClientReadySignal(call)
        val l           = new BidiListener[Response](queue, readySignal)
        call.start(l, headers)
        call.request(1)
        (call, queue, readySignal)
      })
      .flatMap { case (call, queue, readySignal) =>
        val sendAll: F[Unit] =
          requestStream
            .evalMap { req =>
              F.defer {
                if (call.isReady) F.delay(call.sendMessage(req))
                else readySignal.await *> F.delay(call.sendMessage(req))
              }
            }
            .compile
            .drain *> F.delay(call.halfClose())

        val sender: Stream[F, Nothing] = Stream
          .eval(sendAll.handleErrorWith(e => F.delay(call.cancel("Error sending requests", e)) *> F.raiseError[Unit](e)))
          .drain

        streamFromQueue(call, queue)
          .concurrently(sender)
          .onFinalize(F.delay(call.cancel("Stream ended", null)))
      }

  def client[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Request => F[Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    req => unaryRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), req).map(_._1)
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Stream[F, Request] => F[Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    reqs => clientStreamingRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), reqs).map(_._1)
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Request => Stream[F, Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    req => serverStreamingRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), req)
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Stream[F, Request] => Stream[F, Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    reqs => bidiRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), reqs)
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): (Request, Metadata) => F[(Response, Metadata)] = {
    val descriptor = rpc.toMethodDescriptor(service)
    (req, md) =>
      unaryRun(descriptor, options(CallOptions.DEFAULT), md, req).map { case (resp, headers, trailers) =>
        (resp, ClientBackend.mergeMetadata(headers, trailers))
      }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): (Stream[F, Request], Metadata) => F[(Response, Metadata)] = {
    val descriptor = rpc.toMethodDescriptor(service)
    (reqs, md) =>
      clientStreamingRun(descriptor, options(CallOptions.DEFAULT), md, reqs).map { case (resp, headers, trailers) =>
        (resp, ClientBackend.mergeMetadata(headers, trailers))
      }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): (Request, Metadata) => Stream[F, Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    (req, md) => serverStreamingRun(descriptor, options(CallOptions.DEFAULT), md, req)
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): (Stream[F, Request], Metadata) => Stream[F, Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    (reqs, md) => bidiRun(descriptor, options(CallOptions.DEFAULT), md, reqs)
  }
}

object Fs2ClientBackend {
  def apply[F[_]: Async](channel: Channel, dispatcher: Dispatcher[F]): Fs2ClientBackend[F] =
    new Fs2ClientBackend(channel, dispatcher)
}
