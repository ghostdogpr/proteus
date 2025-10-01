package proteus

import java.util.concurrent.TimeUnit

import io.grpc.Metadata
import io.grpc.StatusException
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.protobuf.services.ProtoReflectionServiceV1
import scalapb.zio_grpc.{RequestContext, ZChannel}
import zio.*
import zio.stream.*
import zio.test.*

import proteus.GrpcTestUtils.*
import proteus.client.ZioClientBackend
import proteus.server.{ServerService, ZioServerBackend}

object ZioBackendSpec extends ZIOSpecDefault {

  def processComplexRequestZio(req: ComplexRequest): IO[StatusException, ComplexResponse] =
    ZIO.succeed(processComplexRequest(req))

  val serverService = ServerService(using ZioServerBackend)
    .rpc(complexRpc, processComplexRequestZio)
    .build(testService)

  def processWithMetadataZio(req: MetadataRequest, ctx: RequestContext): IO[StatusException, MetadataResponse] =
    for {
      requestMetadata <- ctx.metadata.get(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER)).map(_.getOrElse("unknown"))
      _               <- ctx.responseMetadata.put(Metadata.Key.of("server-response", Metadata.ASCII_STRING_MARSHALLER), "processed")
    } yield MetadataResponse(req.message.toUpperCase, requestMetadata, "Server processed with metadata")

  val metadataServerService = ServerService(using ZioServerBackend)
    .rpcWithContext(metadataRpc, processWithMetadataZio)
    .build(metadataService)

  def clientStreamingZio(stream: ZStream[Any, StatusException, StreamRequest]): IO[StatusException, StreamResponse] =
    stream.runFold(0)((acc, req) => acc + req.value).map(sum => StreamResponse(sum))

  def serverStreamingZio(req: StreamRequest): ZStream[Any, StatusException, StreamResponse] =
    ZStream.range(1, req.value + 1).map(i => StreamResponse(i))

  def bidiStreamingZio(stream: ZStream[Any, StatusException, StreamRequest]): ZStream[Any, StatusException, StreamResponse] =
    stream.map(req => StreamResponse(req.value * 2))

  val streamingServerService = ServerService(using ZioServerBackend)
    .rpc(clientStreamingRpc, clientStreamingZio)
    .rpc(serverStreamingRpc, serverStreamingZio)
    .rpc(bidiStreamingRpc, bidiStreamingZio)
    .build(streamingService)

  def spec = suite("ZioBackendSpec")(
    test("should discover services via gRPC reflection") {
      assertTrue(testReflection(7010, serverService))
    },
    test("should list services via gRPC reflection client") {
      val port              = 7020
      val reflectionService = ProtoReflectionServiceV1.newInstance
      val server            = NettyServerBuilder.forPort(port).addService(serverService).addService(reflectionService).build().start()
      val channel           = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
      val zChannel          = ZChannel(channel, Seq.empty)
      val clientBackend     = new ZioClientBackend(zChannel)

      val program = for {
        client        <- reflectionClient(clientBackend)
        requestStream  = ZStream.succeed(ServerReflectionRequest("localhost", MessageRequest.ListServices("")))
        responseStream = client(requestStream)
        responses     <- responseStream.runCollect
      } yield responses.headOption
        .flatMap {
          case ServerReflectionResponse(_, _, MessageResponse.ListServicesResponse(services)) =>
            Some(services.map(_.name))
          case _                                                                              => None
        }
        .getOrElse(List.empty)

      program
        .ensuring(ZIO.attempt {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)
        }.ignore)
        .flatMap(serviceNames => assertTrue(serviceNames.contains("test.package.TestService")))
    },
    test("should handle bytes fields via gRPC reflection") {
      val port              = 7021
      val reflectionService = ProtoReflectionServiceV1.newInstance
      val server            = NettyServerBuilder.forPort(port).addService(serverService).addService(reflectionService).build().start()
      val channel           = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
      val zChannel          = ZChannel(channel, Seq.empty)
      val clientBackend     = new ZioClientBackend(zChannel)

      val program = for {
        client    <- reflectionClient(clientBackend)
        // Request file descriptor for the reflection service itself (which should be available)
        responses <- client(
                       ZStream.succeed(
                         ServerReflectionRequest("localhost", MessageRequest.FileContainingSymbol("grpc.reflection.v1.ServerReflection"))
                       )
                     ).runCollect

        result = responses.headOption.collect { case ServerReflectionResponse(_, _, MessageResponse.FileDescriptorResponse(fileDescriptorProto)) =>
                   fileDescriptorProto
                 }
      } yield result

      program
        .ensuring(ZIO.attempt {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)
        }.ignore)
        .flatMap(descriptorBytesOpt => assertTrue(descriptorBytesOpt.exists(_.nonEmpty)))
    },
    test("should handle complex gRPC request/response with zio backend") {
      val port          = 7011
      val server        = NettyServerBuilder.forPort(port).addService(serverService).build().start()
      val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
      val zChannel      = ZChannel(channel, Seq.empty)
      val clientBackend = new ZioClientBackend(zChannel)

      val program = for {
        client <- clientBackend.client(testService, complexRpc)

        response1 <- client(sampleRequest)
        response2 <- client(sampleRequest.copy(contact = ContactMethod.Phone("555-0123", "US"), priority = Priority.Low))
        response3 <- client(sampleRequest.copy(contact = ContactMethod.Slack("my-workspace", "#general"), count = None))
      } yield (response1, response2, response3)

      program
        .flatMap(result => assertTrue(validateComplexResponse(result._1, result._2, result._3)))
        .ensuring(ZIO.attempt {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)
        }.ignore)
    },
    test("should handle client and server metadata") {
      val port          = 7012
      val server        = NettyServerBuilder.forPort(port).addService(metadataServerService).build().start()
      val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
      val zChannel      = ZChannel(channel, Seq.empty)
      val clientBackend = new ZioClientBackend(zChannel)

      val requestMetadata = new Metadata()
      requestMetadata.put(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER), "zio-client-789")
      requestMetadata.put(Metadata.Key.of("user-agent", Metadata.ASCII_STRING_MARSHALLER), "grpc-zio/1.0")

      val program = for {
        client                       <- clientBackend.clientWithMetadata(metadataService, metadataRpc)
        (response, responseMetadata) <- client(MetadataRequest("hello zio metadata"), requestMetadata)
      } yield (response, responseMetadata)

      program
        .flatMap(result => assertTrue(validateMetadataResponse(result._1, result._2, "zio-client-789", "hello zio metadata")))
        .ensuring(ZIO.attempt {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)
        }.ignore)
    },
    test("should handle client streaming") {
      val port          = 7013
      val server        = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
      val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
      val zChannel      = ZChannel(channel, Seq.empty)
      val clientBackend = new ZioClientBackend(zChannel)

      val program = for {
        client       <- clientBackend.client(streamingService, clientStreamingRpc)
        requestStream = ZStream(StreamRequest(1), StreamRequest(2), StreamRequest(3), StreamRequest(4))
        response     <- client(requestStream)
      } yield response

      program
        .flatMap(response => assertTrue(response.result == 10)) // 1+2+3+4 = 10
        .ensuring(ZIO.attempt {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)
        }.ignore)
    },
    test("should handle server streaming") {
      val port          = 7014
      val server        = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
      val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
      val zChannel      = ZChannel(channel, Seq.empty)
      val clientBackend = new ZioClientBackend(zChannel)

      val program = for {
        client        <- clientBackend.client(streamingService, serverStreamingRpc)
        responseStream = client(StreamRequest(5))
        responses     <- responseStream.runCollect
      } yield responses

      program
        .flatMap(responses =>
          assertTrue(responses == Chunk(StreamResponse(1), StreamResponse(2), StreamResponse(3), StreamResponse(4), StreamResponse(5)))
        )
        .ensuring(ZIO.attempt {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)
        }.ignore)
    },
    test("should handle bidirectional streaming") {
      val port          = 7015
      val server        = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
      val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
      val zChannel      = ZChannel(channel, Seq.empty)
      val clientBackend = new ZioClientBackend(zChannel)

      val program = for {
        client        <- clientBackend.client(streamingService, bidiStreamingRpc)
        requestStream  = ZStream(StreamRequest(10), StreamRequest(20), StreamRequest(30))
        responseStream = client(requestStream)
        responses     <- responseStream.runCollect
      } yield responses

      program
        .flatMap(responses => assertTrue(responses == Chunk(StreamResponse(20), StreamResponse(40), StreamResponse(60))))
        .ensuring(ZIO.attempt {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)
        }.ignore)
    }
  )
}
