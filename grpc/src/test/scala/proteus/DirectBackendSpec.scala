package proteus

import java.util.concurrent.TimeUnit

import io.grpc.{Metadata, Status}
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import zio.test.*

import proteus.GrpcTestUtils.*
import proteus.client.DirectClientBackend
import proteus.server.{DirectServerBackend, RequestResponseMetadata, ServerService}

object DirectBackendSpec extends ZIOSpecDefault {

  val testServiceDef = ServerService(using DirectServerBackend)
    .rpc(complexRpc, processComplexRequest)
    .build(testService)

  val metadataServiceDef = ServerService(using DirectServerBackend)
    .rpcWithContext(metadataRpc, processWithMetadata)
    .build(metadataService)

  val failingRpc        = Rpc.unary[MetadataRequest, MetadataResponse]("AlwaysFail")
  val failingService    = Service("test.package", "FailingService").rpc(failingRpc)
  val failingServiceDef = ServerService(using DirectServerBackend)
    .rpc(failingRpc, _ => throw Status.NOT_FOUND.withDescription("missing").asRuntimeException())
    .build(failingService)

  def spec = suite("DirectBackendSpec")(
    test("should discover services via gRPC reflection") {
      assertTrue(testReflection(9000, testServiceDef))
    },
    test("should handle complex gRPC request/response with direct backend") {
      val port    = 9001
      val server  = NettyServerBuilder.forPort(port).addService(testServiceDef).build().start()
      val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
      val client  = new DirectClientBackend(channel).client(complexRpc, testService)

      val testRequest = sampleRequest
      val response    = client(testRequest)

      val testRequest2 = testRequest.copy(contact = ContactMethod.Phone("555-0123", "US"), priority = Priority.Low)
      val response2    = client(testRequest2)

      val testRequest3 = testRequest.copy(contact = ContactMethod.Slack("my-workspace", "#general"), count = None)
      val response3    = client(testRequest3)

      server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
      channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)

      assertTrue(validateComplexResponse(response, response2, response3))
    },
    test("should handle client and server metadata") {
      val port    = 9002
      val server  = NettyServerBuilder.forPort(port).addService(metadataServiceDef).build().start()
      val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
      val client  = new DirectClientBackend(channel).clientWithMetadata(metadataRpc, metadataService)

      val requestMetadata = new Metadata()
      requestMetadata.put(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER), "test-client-123")
      requestMetadata.put(Metadata.Key.of("user-agent", Metadata.ASCII_STRING_MARSHALLER), "grpc-test/1.0")

      val request                      = MetadataRequest("hello metadata")
      val (response, responseMetadata) = client(request, requestMetadata)

      server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
      channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)

      assertTrue(validateMetadataResponse(response, responseMetadata, "test-client-123", "hello metadata"))
    },
    test("should preserve explicit gRPC statuses") {
      val port    = 9003
      val server  = NettyServerBuilder.forPort(port).addService(failingServiceDef).build().start()
      val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
      val client  = new DirectClientBackend(channel).client(failingRpc, failingService)

      val error =
        try {
          client(MetadataRequest("boom"))
          None
        } catch {
          case ex: io.grpc.StatusRuntimeException => Some(ex)
        }

      server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
      channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)

      assertTrue(error.exists(_.getStatus.getCode == Status.Code.NOT_FOUND)) &&
        assertTrue(error.exists(_.getStatus.getDescription == "missing"))
    }
  )
}
