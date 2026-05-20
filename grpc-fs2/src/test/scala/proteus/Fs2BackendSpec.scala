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
import proteus.server.{Fs2ServerBackend, RequestResponseMetadata, ServerService}

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
          val serverService = ServerService(using backend)
            .rpc(complexRpc, processComplexRequestFs2)
            .build(testService)

          IO.pure(testReflection(6000, serverService))
        }
        .unsafeRunSync()

      assertTrue(result)
    },
    test("should list services via gRPC reflection client") {
      val result = Dispatcher
        .parallel[IO]
        .use { dispatcher =>
          val backend       = Fs2ServerBackend[IO](dispatcher)
          val serverService = ServerService(using backend)
            .rpc(complexRpc, processComplexRequestFs2)
            .build(testService)

          val port              = 6020
          val reflectionService = io.grpc.protobuf.services.ProtoReflectionServiceV1.newInstance
          val server            = NettyServerBuilder.forPort(port).addService(serverService).addService(reflectionService).build().start()
          val channel           = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend     = Fs2ClientBackend[IO](channel, dispatcher)

          val client         = reflectionClient(clientBackend)
          val requestStream  = Stream.emit(ServerReflectionRequest("localhost", MessageRequest.ListServices("")))
          val responseStream = client(requestStream)
          val program        =
            for {
              responses <- responseStream.compile.toList
            } yield responses.headOption
              .flatMap {
                case ServerReflectionResponse(_, _, MessageResponse.ListServicesResponse(services)) =>
                  Some(services.map(_.name))
                case _                                                                              => None
              }
              .getOrElse(List.empty)

          program.guarantee {
            IO {
              server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
              channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
            }
          }
        }
        .unsafeRunSync()

      assertTrue(result.contains("test.package.TestService"))
    },
    test("should handle bytes fields via gRPC reflection") {
      val result = Dispatcher
        .parallel[IO]
        .use { dispatcher =>
          val backend       = Fs2ServerBackend[IO](dispatcher)
          val serverService = ServerService(using backend)
            .rpc(complexRpc, processComplexRequestFs2)
            .build(testService)

          val port              = 6021
          val reflectionService = io.grpc.protobuf.services.ProtoReflectionServiceV1.newInstance
          val server            = NettyServerBuilder.forPort(port).addService(serverService).addService(reflectionService).build().start()
          val channel           = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend     = Fs2ClientBackend[IO](channel, dispatcher)

          val client  = reflectionClient(clientBackend)
          val program = for {
            // Request file descriptor for the reflection service itself (which should be available)
            responses <- client(
                           Stream.emit(
                             ServerReflectionRequest("localhost", MessageRequest.FileContainingSymbol("grpc.reflection.v1.ServerReflection"))
                           )
                         ).compile.toList

            result = responses.headOption.collect {
                       case ServerReflectionResponse(_, _, MessageResponse.FileDescriptorResponse(fileDescriptorProto)) =>
                         fileDescriptorProto
                     }
          } yield result

          program.guarantee {
            IO {
              server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
              channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
            }
          }
        }
        .unsafeRunSync()

      assertTrue(result.exists(_.nonEmpty))
    },
    test("should handle complex gRPC request/response with fs2 backend") {
      val result = Dispatcher
        .parallel[IO]
        .use { dispatcher =>
          val backend       = Fs2ServerBackend[IO](dispatcher)
          val serverService = ServerService(using backend)
            .rpc(complexRpc, processComplexRequestFs2)
            .build(testService)

          val port          = 6001
          val server        = NettyServerBuilder.forPort(port).addService(serverService).build().start()
          val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend = Fs2ClientBackend[IO](channel, dispatcher)

          val client1 = clientBackend.client(complexRpc, testService)
          val client2 = clientBackend.client(complexRpc, testService)
          val client3 = clientBackend.client(complexRpc, testService)
          val program = for {

            response1 <- client1(sampleRequest)
            response2 <- client2(sampleRequest.copy(contact = ContactMethod.Phone("555-0123", "US"), priority = Priority.Low))
            response3 <- client3(sampleRequest.copy(contact = ContactMethod.Slack("my-workspace", "#general"), count = None))
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
          val metadataServerService = ServerService(using backend)
            .rpcWithContext(metadataRpc, processWithMetadataFs2)
            .build(metadataService)

          val port          = 6002
          val server        = NettyServerBuilder.forPort(port).addService(metadataServerService).build().start()
          val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend = Fs2ClientBackend[IO](channel, dispatcher)

          val requestMetadata = new Metadata()
          requestMetadata.put(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER), "fs2-client-101")
          requestMetadata.put(Metadata.Key.of("user-agent", Metadata.ASCII_STRING_MARSHALLER), "grpc-fs2/1.0")

          val client  = clientBackend.clientWithMetadata(metadataRpc, metadataService)
          val program = for {
            result <- client(MetadataRequest("hello fs2 metadata"), requestMetadata)
          } yield result

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
          val streamingServerService = ServerService(using backend)
            .rpc(clientStreamingRpc, clientStreamingFs2)
            .build(clientStreamingService)

          val port          = 6003
          val server        = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
          val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend = Fs2ClientBackend[IO](channel, dispatcher)

          val client        = clientBackend.client(clientStreamingRpc, clientStreamingService)
          val requestStream = Stream(StreamRequest(1), StreamRequest(2), StreamRequest(3), StreamRequest(4))
          val program       = for {
            response <- client(requestStream)
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
          val streamingServerService = ServerService(using backend)
            .rpc(serverStreamingRpc, serverStreamingFs2)
            .build(serverStreamingService)

          val port          = 6004
          val server        = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
          val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend = Fs2ClientBackend[IO](channel, dispatcher)

          val client         = clientBackend.client(serverStreamingRpc, serverStreamingService)
          val responseStream = client(StreamRequest(5))
          val program        = for {
            responses <- responseStream.compile.toList
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
          val streamingServerService = ServerService(using backend)
            .rpc(bidiStreamingRpc, bidiStreamingFs2)
            .build(bidiStreamingService)

          val port          = 6005
          val server        = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
          val channel       = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
          val clientBackend = Fs2ClientBackend[IO](channel, dispatcher)

          val client         = clientBackend.client(bidiStreamingRpc, bidiStreamingService)
          val requestStream  = Stream(StreamRequest(10), StreamRequest(20), StreamRequest(30))
          val responseStream = client(requestStream)
          val program        = for {
            responses <- responseStream.compile.toList
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
