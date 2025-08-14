package proteus

import java.util.concurrent.TimeUnit
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import zio.test.*
import proteus.client.DirectClientBackend
import proteus.server.{DirectServerBackend, ServerServiceBuilder}
import GrpcTestUtils.*

object DirectBackendSpec extends ZIOSpecDefault {

  val serverService = ServerServiceBuilder(using DirectServerBackend)
    .rpc(complexRpc, processComplexRequest)
    .build(testService)

  def spec = suite("DirectBackendSpec")(
    test("should handle complex gRPC request/response with direct backend") {
      val port = 9999
      val server = NettyServerBuilder.forPort(port).addService(serverService.definition).build().start()
      val channel = NettyChannelBuilder.forAddress("localhost", 9999).usePlaintext().build()
      val client = new DirectClientBackend(channel).client(testService, complexRpc)

      val testRequest = sampleRequest
      val response = client(testRequest)

      val testRequest2 = testRequest.copy(contact = ContactMethod.Phone("555-0123", "US"), priority = Priority.Low)
      val response2 = client(testRequest2)

      val testRequest3 = testRequest.copy(contact = ContactMethod.Slack("my-workspace", "#general"), count = None)
      val response3 = client(testRequest3)

      server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
      channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)

      assertTrue(validateComplexResponse(response, response2, response3))
    },
    test("should discover services via gRPC reflection") {
      assertTrue(testReflection(9998, serverService.definition))
    }
  )
}