package proteus

import java.util.concurrent.TimeUnit

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*

import io.grpc.Metadata
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import zio.test.*

import proteus.GrpcTestUtils.*
import proteus.client.FutureClientBackend
import proteus.server.{FutureServerBackend, RequestResponseMetadata, ServerServiceBuilder}

object FutureBackendSpec extends ZIOSpecDefault {

  def processComplexRequestFuture(req: ComplexRequest): Future[ComplexResponse] =
    Future.successful(processComplexRequest(req))

  def processWithMetadataFuture(req: MetadataRequest, ctx: RequestResponseMetadata): Future[MetadataResponse] =
    Future.successful(processWithMetadata(req, ctx))

  val serverService = ServerServiceBuilder(using FutureServerBackend)
    .rpc(complexRpc, processComplexRequestFuture)
    .build(testService)

  val metadataServerService = ServerServiceBuilder(using FutureServerBackend)
    .rpcWithContext(metadataRpc, processWithMetadataFuture)
    .build(metadataService)

  def spec = suite("FutureBackendSpec")(
    test("should discover services via gRPC reflection") {
      assertTrue(testReflection(8998, serverService.definition))
    },
    test("should handle complex gRPC request/response with future backend") {
      val port         = 8999
      val server       = NettyServerBuilder.forPort(port).addService(serverService.definition).build().start()
      val channel      = NettyChannelBuilder.forAddress("localhost", 8999).usePlaintext().build()
      val clientFuture = new FutureClientBackend(channel).client(testService, complexRpc)
      val client       = Await.result(clientFuture, 5.seconds)

      val testRequest = sampleRequest
      val response    = Await.result(client(testRequest), 5.seconds)

      val testRequest2 = testRequest.copy(contact = ContactMethod.Phone("555-0123", "US"), priority = Priority.Low)
      val response2    = Await.result(client(testRequest2), 5.seconds)

      val testRequest3 = testRequest.copy(contact = ContactMethod.Slack("my-workspace", "#general"), count = None)
      val response3    = Await.result(client(testRequest3), 5.seconds)

      server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
      channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)

      assertTrue(validateComplexResponse(response, response2, response3))
    },
    test("should handle client and server metadata") {
      val port         = 8997
      val server       = NettyServerBuilder.forPort(port).addService(metadataServerService.definition).build().start()
      val channel      = NettyChannelBuilder.forAddress("localhost", 8997).usePlaintext().build()
      val clientFuture = new FutureClientBackend(channel).clientWithMetadata(metadataService, metadataRpc)
      val client       = Await.result(clientFuture, 5.seconds)

      val requestMetadata = new Metadata()
      requestMetadata.put(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER), "future-client-456")
      requestMetadata.put(Metadata.Key.of("user-agent", Metadata.ASCII_STRING_MARSHALLER), "grpc-future/1.0")

      val request                      = MetadataRequest("hello future metadata")
      val (response, responseMetadata) = Await.result(client(request, requestMetadata), 5.seconds)

      server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
      channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)

      assertTrue(validateMetadataResponse(response, responseMetadata, "future-client-456", "hello future metadata"))
    }
  )
}
