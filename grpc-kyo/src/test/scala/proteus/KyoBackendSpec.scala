package proteus

import java.util.concurrent.TimeUnit

import io.grpc.{Metadata, Status, StatusException}
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.protobuf.services.ProtoReflectionServiceV1
import kyo.*
import kyo.AllowUnsafe.embrace.danger
import zio.test.*

import proteus.GrpcTestUtils.*
import proteus.client.KyoClientBackend
import proteus.server.{GrpcContext, KyoServerBackend, ServerInterceptor, ServerService}

object KyoBackendSpec extends ZIOSpecDefault {

  private def runKyo[A](effect: A < (Async & Abort[StatusException])): A =
    KyoApp.Unsafe.runAndBlock(Duration.fromUnits(30, Duration.Units.Seconds))(effect) match {
      case kyo.Result.Success(a) => a
      case kyo.Result.Failure(e) => throw e
      case kyo.Result.Panic(e)   => throw e
    }

  given KyoServerBackend[Async & Abort[StatusException], GrpcContext] = KyoServerBackend()

  def processComplexRequestKyo(req: ComplexRequest): ComplexResponse < (Async & Abort[StatusException]) =
    processComplexRequest(req)

  val serverService = ServerService(using summon[KyoServerBackend[Async & Abort[StatusException], GrpcContext]])
    .rpc(complexRpc, processComplexRequestKyo)
    .build(testService)

  def processWithMetadataKyo(req: MetadataRequest, ctx: GrpcContext): MetadataResponse < (Async & Abort[StatusException]) = {
    val requestMetadata =
      Option(ctx.requestMetadata.get(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER))).getOrElse("unknown")
    ctx.responseMetadata.put(Metadata.Key.of("server-response", Metadata.ASCII_STRING_MARSHALLER), "processed")
    MetadataResponse(req.message.toUpperCase, requestMetadata, "Server processed with metadata")
  }

  val metadataServerService = ServerService(using summon[KyoServerBackend[Async & Abort[StatusException], GrpcContext]])
    .rpcWithContext(metadataRpc, processWithMetadataKyo)
    .build(metadataService)

  def clientStreamingKyo(stream: Stream[StreamRequest, Async & Abort[StatusException]]): StreamResponse < (Async & Abort[StatusException]) =
    stream.fold(0)((sum, req) => sum + req.value).map(StreamResponse(_))

  def serverStreamingKyo(req: StreamRequest): Stream[StreamResponse, Async & Abort[StatusException]] =
    Stream.init((1 to req.value).map(i => StreamResponse(i)))

  def bidiStreamingKyo(stream: Stream[StreamRequest, Async & Abort[StatusException]]): Stream[StreamResponse, Async & Abort[StatusException]] =
    stream.map(req => StreamResponse(req.value * 2))

  val streamingServerService = ServerService(using summon[KyoServerBackend[Async & Abort[StatusException], GrpcContext]])
    .rpc(clientStreamingRpc, clientStreamingKyo)
    .rpc(serverStreamingRpc, serverStreamingKyo)
    .rpc(bidiStreamingRpc, bidiStreamingKyo)
    .build(streamingService)

  def spec = suite("KyoBackendSpec")(
    test("should discover services via gRPC reflection") {
      assertTrue(testReflection(7110, serverService))
    },
    test("should list services via gRPC reflection client") {
      val port              = 7120
      val reflectionService = ProtoReflectionServiceV1.newInstance
      val server            = NettyServerBuilder.forPort(port).addService(serverService).addService(reflectionService).build().start()
      val channel           = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

      try {
        val clientBackend  = KyoClientBackend(channel)
        val client         = reflectionClient(clientBackend)
        val requestStream  = Stream.init(List(ServerReflectionRequest("localhost", MessageRequest.ListServices(""))))
        val responseStream = client(requestStream)
        val responses      = runKyo(responseStream.run).toList
        val serviceNames   = responses.headOption
          .flatMap {
            case ServerReflectionResponse(_, _, MessageResponse.ListServicesResponse(services)) => Some(services.map(_.name))
            case _                                                                              => None
          }
          .getOrElse(List.empty)

        assertTrue(serviceNames.contains("test.package.TestService"))
      } finally {
        server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
      }
    },
    test("should handle bytes fields via gRPC reflection") {
      val port              = 7121
      val reflectionService = ProtoReflectionServiceV1.newInstance
      val server            = NettyServerBuilder.forPort(port).addService(serverService).addService(reflectionService).build().start()
      val channel           = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

      try {
        val clientBackend = KyoClientBackend(channel)
        val client        = reflectionClient(clientBackend)
        val requestStream =
          Stream.init(List(ServerReflectionRequest("localhost", MessageRequest.FileContainingSymbol("grpc.reflection.v1.ServerReflection"))))
        val responses     = runKyo(client(requestStream).run).toList
        val descriptor    = responses.headOption.collect {
          case ServerReflectionResponse(_, _, MessageResponse.FileDescriptorResponse(fileDescriptorProto)) => fileDescriptorProto
        }

        assertTrue(descriptor.exists(_.nonEmpty))
      } finally {
        server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
      }
    },
    test("should handle complex gRPC request/response with kyo backend") {
      val port    = 7111
      val server  = NettyServerBuilder.forPort(port).addService(serverService).build().start()
      val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

      try {
        val clientBackend = KyoClientBackend(channel)
        val client        = clientBackend.client(complexRpc, testService)
        val response1     = runKyo(client(sampleRequest))
        val response2     = runKyo(client(sampleRequest.copy(contact = ContactMethod.Phone("555-0123", "US"), priority = Priority.Low)))
        val response3     = runKyo(client(sampleRequest.copy(contact = ContactMethod.Slack("my-workspace", "#general"), count = None)))

        assertTrue(validateComplexResponse(response1, response2, response3))
      } finally {
        server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
      }
    },
    test("should handle client and server metadata") {
      val port    = 7112
      val server  = NettyServerBuilder.forPort(port).addService(metadataServerService).build().start()
      val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

      try {
        val clientBackend   = KyoClientBackend(channel)
        val requestMetadata = new Metadata()
        requestMetadata.put(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER), "kyo-client-789")
        requestMetadata.put(Metadata.Key.of("user-agent", Metadata.ASCII_STRING_MARSHALLER), "grpc-kyo/1.0")

        val client                       = clientBackend.clientWithMetadata(metadataRpc, metadataService)
        val (response, responseMetadata) = runKyo(client(MetadataRequest("hello kyo metadata"), requestMetadata))

        assertTrue(validateMetadataResponse(response, responseMetadata, "kyo-client-789", "hello kyo metadata"))
      } finally {
        server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
      }
    },
    test("should handle client streaming") {
      val port    = 7113
      val server  = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
      val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

      try {
        val clientBackend = KyoClientBackend(channel)
        val client        = clientBackend.client(clientStreamingRpc, streamingService)
        val requestStream = Stream.init(List(StreamRequest(1), StreamRequest(2), StreamRequest(3), StreamRequest(4)))
        val response      = runKyo(client(requestStream))

        assertTrue(response.result == 10)
      } finally {
        server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
      }
    },
    test("should handle server streaming") {
      val port    = 7114
      val server  = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
      val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

      try {
        val clientBackend = KyoClientBackend(channel)
        val client        = clientBackend.client(serverStreamingRpc, streamingService)
        val responses     = runKyo(client(StreamRequest(5)).run).toList

        assertTrue(responses == List(StreamResponse(1), StreamResponse(2), StreamResponse(3), StreamResponse(4), StreamResponse(5)))
      } finally {
        server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
      }
    },
    test("should handle bidirectional streaming") {
      val port    = 7115
      val server  = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
      val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

      try {
        val clientBackend = KyoClientBackend(channel)
        val client        = clientBackend.client(bidiStreamingRpc, streamingService)
        val requestStream = Stream.init(List(StreamRequest(10), StreamRequest(20), StreamRequest(30)))
        val responses     = runKyo(client(requestStream).run).toList

        assertTrue(responses == List(StreamResponse(20), StreamResponse(40), StreamResponse(60)))
      } finally {
        server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
      }
    },
    test("should handle server interceptor that changes the effect type") {
      def toStatusException[A, S](effect: A < (S & Abort[String])): A < (S & Abort[StatusException]) =
        Abort.run[String](effect).map {
          case kyo.Result.Success(a) => a
          case kyo.Result.Failure(e) => Abort.fail(Status.INTERNAL.withDescription(e).asException())
          case kyo.Result.Panic(e)   => Abort.panic(e)
        }

      def toMessage[A, S](effect: A < (S & Abort[StatusException])): A < (S & Abort[String]) =
        Abort.run[StatusException](effect).map {
          case kyo.Result.Success(a) => a
          case kyo.Result.Failure(e) => Abort.fail(e.getMessage)
          case kyo.Result.Panic(e)   => Abort.panic(e)
        }

      val backend            = KyoServerBackend(
        new ServerInterceptor[
          [A] =>> A < (Async & Abort[StatusException]),
          [A] =>> A < (Async & Abort[String]),
          [A] =>> Stream[A, Async & Abort[StatusException]],
          [A] =>> Stream[A, Async & Abort[String]],
          GrpcContext,
          GrpcContext
        ] {
          def unary[Req: ProtobufCodec, Resp: ProtobufCodec](
            io: GrpcContext => Resp < (Async & Abort[String])
          ): Req => GrpcContext => Resp < (Async & Abort[StatusException]) =
            _ => ctx => toStatusException(io(ctx))

          def clientStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](
            io: Stream[Req, Async & Abort[String]] => GrpcContext => Resp < (Async & Abort[String])
          ): Stream[Req, Async & Abort[StatusException]] => GrpcContext => Resp < (Async & Abort[StatusException]) =
            stream => ctx => toStatusException(io(Stream(toMessage(stream.emit)))(ctx))

          def serverStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](
            io: GrpcContext => Stream[Resp, Async & Abort[String]]
          ): Req => GrpcContext => Stream[Resp, Async & Abort[StatusException]] =
            _ => ctx => Stream(toStatusException(io(ctx).emit))

          def bidiStreaming[Req: ProtobufCodec, Resp: ProtobufCodec](
            io: Stream[Req, Async & Abort[String]] => GrpcContext => Stream[Resp, Async & Abort[String]]
          ): Stream[Req, Async & Abort[StatusException]] => GrpcContext => Stream[Resp, Async & Abort[StatusException]] =
            stream => ctx => Stream(toStatusException(io(Stream(toMessage(stream.emit)))(ctx).emit))
        },
        16
      )
      val interceptorService = ServerService(using backend)
        .rpc(complexRpc, _ => Abort.fail("boom"))
        .build(testService)

      val port    = 7116
      val server  = NettyServerBuilder.forPort(port).addService(interceptorService).build().start()
      val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

      try {
        val clientBackend = KyoClientBackend(channel)
        val client        = clientBackend.client(complexRpc, testService)
        val description   =
          try { runKyo(client(sampleRequest)); None }
          catch { case e: StatusException => Some(e.getStatus.getDescription) }

        assertTrue(description.contains("boom"))
      } finally {
        server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
      }
    }
  )
}
