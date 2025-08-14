package proteus

import java.util.concurrent.TimeUnit
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import zio.test.*
import proteus.client.FutureClientBackend
import proteus.server.{FutureServerBackend, ServerServiceBuilder}
import GrpcTestUtils.*

object FutureBackendSpec extends ZIOSpecDefault {

  def processComplexRequestFuture(req: ComplexRequest): Future[ComplexResponse] =
    Future.successful(processComplexRequest(req))

  val serverService = ServerServiceBuilder(using FutureServerBackend)
    .rpc(complexRpc, processComplexRequestFuture)
    .build(testService)

  def spec = suite("FutureBackendSpec")(
    test("should handle complex gRPC request/response with future backend") {
      val port = 8999
      val server = NettyServerBuilder.forPort(port).addService(serverService.definition).build().start()
      val channel = NettyChannelBuilder.forAddress("localhost", 8999).usePlaintext().build()
      val clientFuture = new FutureClientBackend(channel).client(testService, complexRpc)
      val client = Await.result(clientFuture, 5.seconds)

      val testRequest = sampleRequest
      val response = Await.result(client(testRequest), 5.seconds)

      val testRequest2 = testRequest.copy(contact = ContactMethod.Phone("555-0123", "US"), priority = Priority.Low)
      val response2 = Await.result(client(testRequest2), 5.seconds)

      val testRequest3 = testRequest.copy(contact = ContactMethod.Slack("my-workspace", "#general"), count = None)
      val response3 = Await.result(client(testRequest3), 5.seconds)

      server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
      channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)

      assertTrue(validateComplexResponse(response, response2, response3))
    },
    test("should discover services via gRPC reflection") {
      assertTrue(testReflection(8998, serverService.definition))
    }
  )
}