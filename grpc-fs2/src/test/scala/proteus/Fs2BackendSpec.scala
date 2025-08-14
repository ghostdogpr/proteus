package proteus

import java.util.concurrent.TimeUnit

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import fs2.Stream
import io.grpc.Metadata
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import zio.test.*

import proteus.GrpcTestUtils.*
import proteus.client.Fs2ClientBackend
import proteus.server.{Fs2ServerBackend, RequestResponseMetadata, ServerServiceBuilder}

object Fs2BackendSpec extends ZIOSpecDefault {

  def processComplexRequestFs2(req: ComplexRequest): IO[ComplexResponse] =
    IO.pure(processComplexRequest(req))

  def processWithMetadataFs2(req: MetadataRequest, ctx: RequestResponseMetadata): IO[MetadataResponse] =
    IO.pure(processWithMetadata(req, ctx))

  def clientStreamingFs2(stream: Stream[IO, StreamRequest]): IO[StreamResponse] =
    stream.fold(0)((acc, req) => acc + req.value).compile.lastOrError.map(sum => StreamResponse(sum))

  def serverStreamingFs2(req: StreamRequest): Stream[IO, StreamResponse] =
    Stream.range(1, req.value + 1).map(i => StreamResponse(i))

  def bidiStreamingFs2(stream: Stream[IO, StreamRequest]): Stream[IO, StreamResponse] =
    stream.map(req => StreamResponse(req.value * 2))

  def spec = suite("Fs2BackendSpec")(
    test("should discover services via gRPC reflection") {
      val result = Dispatcher
        .parallel[IO]
        .use { dispatcher =>
          val backend       = Fs2ServerBackend[IO](dispatcher)
          val serverService = ServerServiceBuilder(using backend)
            .rpc(complexRpc, processComplexRequestFs2)
            .build(testService)

          IO.pure(testReflection(6000, serverService.definition))
        }
        .unsafeRunSync()

      assertTrue(result)
    },
    test("should handle complex gRPC request/response with fs2 backend") {
      val result = Dispatcher
        .parallel[IO]
        .use { dispatcher =>
          val backend       = Fs2ServerBackend[IO](dispatcher)
          val serverService = ServerServiceBuilder(using backend)
            .rpc(complexRpc, processComplexRequestFs2)
            .build(testService)

          val port          = 6001
          val server        = NettyServerBuilder.forPort(port).addService(serverService.definition).build().start()
          val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend = new Fs2ClientBackend[IO](channel, dispatcher)

          val program = for {
            clientFactory1 <- clientBackend.client(testService, complexRpc)
            clientFactory2 <- clientBackend.client(testService, complexRpc)
            clientFactory3 <- clientBackend.client(testService, complexRpc)

            response1 <- clientFactory1(sampleRequest)
            response2 <- clientFactory2(sampleRequest.copy(contact = ContactMethod.Phone("555-0123", "US"), priority = Priority.Low))
            response3 <- clientFactory3(sampleRequest.copy(contact = ContactMethod.Slack("my-workspace", "#general"), count = None))
          } yield (response1, response2, response3)

          program.guarantee {
            IO {
              server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
              channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
            }
          }
        }
        .unsafeRunSync()

      assertTrue(validateComplexResponse(result._1, result._2, result._3))
    },
    test("should handle client and server metadata") {
      val result = Dispatcher
        .parallel[IO]
        .use { dispatcher =>
          val backend               = Fs2ServerBackend[IO](dispatcher)
          val metadataServerService = ServerServiceBuilder(using backend)
            .rpcWithContext(metadataRpc, processWithMetadataFs2)
            .build(metadataService)

          val port          = 6002
          val server        = NettyServerBuilder.forPort(port).addService(metadataServerService.definition).build().start()
          val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend = new Fs2ClientBackend[IO](channel, dispatcher)

          val requestMetadata = new Metadata()
          requestMetadata.put(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER), "fs2-client-101")
          requestMetadata.put(Metadata.Key.of("user-agent", Metadata.ASCII_STRING_MARSHALLER), "grpc-fs2/1.0")

          val program = for {
            clientFactory                <- clientBackend.clientWithMetadata(metadataService, metadataRpc)
            (response, responseMetadata) <- clientFactory(MetadataRequest("hello fs2 metadata"), requestMetadata)
          } yield (response, responseMetadata)

          program.guarantee {
            IO {
              server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
              channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
            }
          }
        }
        .unsafeRunSync()

      assertTrue(validateMetadataResponse(result._1, result._2, "fs2-client-101", "hello fs2 metadata"))
    },
    test("should handle client streaming") {
      val result = Dispatcher
        .parallel[IO]
        .use { dispatcher =>
          val backend                = Fs2ServerBackend[IO](dispatcher)
          val clientStreamingService = Service("ClientStreamingService").rpc(clientStreamingRpc)
          val streamingServerService = ServerServiceBuilder(using backend)
            .rpc(clientStreamingRpc, clientStreamingFs2)
            .build(clientStreamingService)

          val port          = 6003
          val server        = NettyServerBuilder.forPort(port).addService(streamingServerService.definition).build().start()
          val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend = new Fs2ClientBackend[IO](channel, dispatcher)

          val program = for {
            clientFactory <- clientBackend.client(clientStreamingService, clientStreamingRpc)
            requestStream  = Stream(StreamRequest(1), StreamRequest(2), StreamRequest(3), StreamRequest(4))
            response      <- clientFactory(requestStream)
          } yield response

          program.guarantee {
            IO {
              server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
              channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
            }
          }
        }
        .unsafeRunSync()

      assertTrue(result.result == 10) // 1+2+3+4 = 10
    },
    test("should handle server streaming") {
      val result = Dispatcher
        .parallel[IO]
        .use { dispatcher =>
          val backend                = Fs2ServerBackend[IO](dispatcher)
          val serverStreamingService = Service("ServerStreamingService").rpc(serverStreamingRpc)
          val streamingServerService = ServerServiceBuilder(using backend)
            .rpc(serverStreamingRpc, serverStreamingFs2)
            .build(serverStreamingService)

          val port          = 6004
          val server        = NettyServerBuilder.forPort(port).addService(streamingServerService.definition).build().start()
          val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend = new Fs2ClientBackend[IO](channel, dispatcher)

          val program = for {
            clientFactory <- clientBackend.client(serverStreamingService, serverStreamingRpc)
            responseStream = clientFactory(StreamRequest(5))
            responses     <- responseStream.compile.toList
          } yield responses

          program.guarantee {
            IO {
              server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
              channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
            }
          }
        }
        .unsafeRunSync()

      assertTrue(result == List(StreamResponse(1), StreamResponse(2), StreamResponse(3), StreamResponse(4), StreamResponse(5)))
    },
    test("should handle bidirectional streaming") {
      val result = Dispatcher
        .parallel[IO]
        .use { dispatcher =>
          val backend                = Fs2ServerBackend[IO](dispatcher)
          val bidiStreamingService   = Service("BidiStreamingService").rpc(bidiStreamingRpc)
          val streamingServerService = ServerServiceBuilder(using backend)
            .rpc(bidiStreamingRpc, bidiStreamingFs2)
            .build(bidiStreamingService)

          val port          = 6005
          val server        = NettyServerBuilder.forPort(port).addService(streamingServerService.definition).build().start()
          val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend = new Fs2ClientBackend[IO](channel, dispatcher)

          val program = for {
            clientFactory <- clientBackend.client(bidiStreamingService, bidiStreamingRpc)
            requestStream  = Stream(StreamRequest(10), StreamRequest(20), StreamRequest(30))
            responseStream = clientFactory(requestStream)
            responses     <- responseStream.compile.toList
          } yield responses

          program.guarantee {
            IO {
              server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
              channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
            }
          }
        }
        .unsafeRunSync()

      assertTrue(result == List(StreamResponse(20), StreamResponse(40), StreamResponse(60)))
    }
  )
}
