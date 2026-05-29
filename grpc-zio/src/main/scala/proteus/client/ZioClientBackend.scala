package proteus
package client

import java.util.concurrent.atomic.AtomicReference

import io.grpc.*
import zio.*
import zio.stream.*

/**
  * A client backend that wraps results in a ZIO effect.
  * Streaming is supported using [[zio.stream.ZStream]].
  *
  * @param channel the gRPC channel used to issue calls.
  * @param runtime the ZIO runtime to use for running ZIO effects.
  * @param prefetchN initial in-flight response window for server-streaming / bidi RPCs; the window is refilled one message at a time as the consumer pulls.
  */
class ZioClientBackend(channel: Channel, runtime: Runtime[Any], prefetchN: Int)
  extends ClientBackend[IO[StatusException, *], ZStream[Any, StatusException, *], NoTag] {

  private val prefetch: Int = math.max(prefetchN, 1)

  private class UnaryListener[Response] extends ClientCall.Listener[Response] {
    private val headersRef                                                = new AtomicReference[Metadata](null)
    private val messageRef                                                = new AtomicReference[Response]()
    val promise: Promise[StatusException, (Response, Metadata, Metadata)] =
      Unsafe.unsafely(Promise.unsafe.make[StatusException, (Response, Metadata, Metadata)](FiberId.None))

    override def onHeaders(headers: Metadata): Unit =
      headersRef.set(headers)

    override def onMessage(message: Response): Unit =
      messageRef.set(message)

    override def onClose(status: Status, trailers: Metadata): Unit = {
      val effect: IO[StatusException, Boolean] =
        if (status.isOk) {
          val msg = messageRef.get()
          if (msg == null) promise.fail(Status.INTERNAL.withDescription("No data received").asException())
          else promise.succeed((msg, headersRef.get(), trailers)) // headers may be null; consumers handle it
        } else {
          promise.fail(new StatusException(status, trailers))
        }
      Unsafe.unsafely(runtime.unsafe.run(effect).getOrThrowFiberFailure(): Unit)
    }
  }

  private class StreamingListener[Response](queue: Queue[Take[StatusException, Response]]) extends ClientCall.Listener[Response] {
    protected def offer(a: Take[StatusException, Response]): Unit =
      Unsafe.unsafely(runtime.unsafe.run(queue.offer(a)).getOrThrowFiberFailure(): Unit)

    override def onHeaders(headers: Metadata): Unit = ()

    override def onMessage(message: Response): Unit = offer(Take.single(message))

    override def onClose(status: Status, trailers: Metadata): Unit =
      if (status.isOk) offer(Take.end) else offer(Take.fail(new StatusException(status, trailers)))
  }

  final private class ClientReadySignal(call: ClientCall[?, ?]) {
    private def mkPromise(): Promise[Nothing, Unit] =
      Unsafe.unsafely(Promise.unsafe.make[Nothing, Unit](FiberId.None))

    private val ref = new AtomicReference[Promise[Nothing, Unit]](mkPromise())

    val await: UIO[Unit] = ZIO.suspendSucceed {
      // Capture promise BEFORE the isReady check so a concurrent signal() is never lost.
      val current = ref.get()
      if (call.isReady) ZIO.unit
      else current.await
    }

    def signal(): Unit = {
      val old = ref.getAndSet(mkPromise())
      Unsafe.unsafely(runtime.unsafe.run(old.succeed(())).getOrThrowFiberFailure(): Unit)
    }
  }

  final private class BidiListener[Response](
    queue: Queue[Take[StatusException, Response]],
    val readySignal: ClientReadySignal
  ) extends StreamingListener[Response](queue) {
    override def onReady(): Unit = readySignal.signal()
  }

  private def unaryRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    request: Request
  ): IO[StatusException, (Response, Metadata, Metadata)] = ZIO.suspendSucceed {
    val call     = channel.newCall(descriptor, options)
    val listener = new UnaryListener[Response]
    ZIO
      .attempt {
        call.start(listener, headers)
        call.request(1)
        call.sendMessage(request)
        call.halfClose()
      }
      .foldZIO(
        e => ZIO.succeed(call.cancel("Failed to send unary request", e)) *> ZIO.fail(ClientBackend.toStatusException(e)),
        _ => listener.promise.await.onInterrupt(ZIO.succeed(call.cancel("Interrupted", null)))
      )
  }

  private def streamFromQueue[Response](
    call: ClientCall[?, Response],
    queue: Queue[Take[StatusException, Response]]
  ): ZStream[Any, StatusException, Response] =
    ZStream
      .fromQueue(queue, prefetch)
      .flattenTake
      .tapChunks(chunk => ZIO.succeed(if (chunk.nonEmpty) call.request(chunk.size)))
      .ensuring(ZIO.succeed(call.cancel("Stream ended", null)))

  private def serverStreamingRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    request: Request
  ): ZStream[Any, StatusException, Response] =
    ZStream.unwrap(
      ZIO.suspendSucceed {
        val call     = channel.newCall(descriptor, options)
        val queue    = Unsafe.unsafely(Queue.unsafe.unbounded[Take[StatusException, Response]](FiberId.None))
        val listener = new StreamingListener[Response](queue)
        ZIO
          .attempt {
            call.start(listener, headers)
            call.request(prefetch)
            call.sendMessage(request)
            call.halfClose()
          }
          .foldZIO(
            e => ZIO.succeed(call.cancel("Failed to start server-streaming", e)).as(ZStream.fail(ClientBackend.toStatusException(e))),
            _ => ZIO.succeed(streamFromQueue(call, queue))
          )
      }
    )

  private def clientStreamingRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    requestStream: ZStream[Any, StatusException, Request]
  ): IO[StatusException, (Response, Metadata, Metadata)] = ZIO.suspendSucceed {
    val call        = channel.newCall(descriptor, options)
    val readySignal = new ClientReadySignal(call)
    val listener    = new UnaryListener[Response] {
      override def onReady(): Unit = readySignal.signal()
    }
    call.start(listener, headers)
    call.request(1)

    val sendAll = requestStream.runForeach { req =>
      if (call.isReady) ZIO.succeed(call.sendMessage(req))
      else readySignal.await *> ZIO.succeed(call.sendMessage(req))
    } *> ZIO.succeed(call.halfClose())

    // Sender surfaces failures via the response promise so promise.await can return them
    // even if the server hasn't closed; on success the server completes the promise via onClose.
    val send: UIO[Unit] = sendAll.catchAll { e =>
      ZIO.succeed(call.cancel("Error sending requests", e)) <* listener.promise.fail(e)
    }

    // Server may respond and close before the request stream is fully drained — interrupt
    // the sender as soon as the response arrives, to avoid pushing into a closed call.
    ZIO
      .uninterruptibleMask { restore =>
        send.forkDaemon.flatMap { senderFiber =>
          restore(listener.promise.await).onExit(_ => senderFiber.interrupt)
        }
      }
      .onInterrupt(ZIO.succeed(call.cancel("Interrupted", null)))
  }

  private def bidiRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    requestStream: ZStream[Any, StatusException, Request]
  ): ZStream[Any, StatusException, Response] =
    ZStream.unwrap(
      ZIO.suspendSucceed {
        val call        = channel.newCall(descriptor, options)
        val queue       = Unsafe.unsafely(Queue.unsafe.unbounded[Take[StatusException, Response]](FiberId.None))
        val readySignal = new ClientReadySignal(call)
        val listener    = new BidiListener[Response](queue, readySignal)
        call.start(listener, headers)
        call.request(prefetch)

        val sendAll = (requestStream.runForeach { req =>
          if (call.isReady) ZIO.succeed(call.sendMessage(req))
          else readySignal.await *> ZIO.succeed(call.sendMessage(req))
        } *> ZIO.succeed(call.halfClose()))
          .catchAll(e => ZIO.succeed(call.cancel("Error sending requests", e)) *> ZIO.fail(e))

        // HaltStrategy.Right: stop the sender if the server closes the response stream first.
        ZIO.succeed(
          ZStream
            .fromZIO(sendAll)
            .drain
            .merge(streamFromQueue(call, queue), ZStream.HaltStrategy.Right)
        )
      }
    )

  def client[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Request => IO[StatusException, Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    req => unaryRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), req).map(_._1)
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type], NoTag[Request]): ZStream[Any, StatusException, Request] => IO[StatusException, Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    reqs => clientStreamingRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), reqs).map(_._1)
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type], NoTag[Response]): Request => ZStream[Any, StatusException, Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    req => serverStreamingRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), req)
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(
    using HasRpc[Rpcs, rpc.type],
    NoTag[Request],
    NoTag[Response]
  ): ZStream[Any, StatusException, Request] => ZStream[Any, StatusException, Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    reqs => bidiRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), reqs)
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): (Request, Metadata) => IO[StatusException, (Response, Metadata)] = {
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
  )(using HasRpc[Rpcs, rpc.type], NoTag[Request]): (ZStream[Any, StatusException, Request], Metadata) => IO[StatusException, (Response, Metadata)] = {
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
  )(using HasRpc[Rpcs, rpc.type], NoTag[Response]): (Request, Metadata) => ZStream[Any, StatusException, Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    (req, md) => serverStreamingRun(descriptor, options(CallOptions.DEFAULT), md, req)
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(
    using HasRpc[Rpcs, rpc.type],
    NoTag[Request],
    NoTag[Response]
  ): (ZStream[Any, StatusException, Request], Metadata) => ZStream[Any, StatusException, Response] = {
    val descriptor = rpc.toMethodDescriptor(service)
    (reqs, md) => bidiRun(descriptor, options(CallOptions.DEFAULT), md, reqs)
  }
}

object ZioClientBackend {

  /**
    * Creates a new ZIO client backend.
    *
    * @param channel the gRPC channel used to issue calls.
    * @param prefetchN initial in-flight response window for streaming RPCs.
    */
  def apply(channel: Channel, prefetchN: Int = 16): ZioClientBackend =
    new ZioClientBackend(channel, Runtime.default, prefetchN)
}
