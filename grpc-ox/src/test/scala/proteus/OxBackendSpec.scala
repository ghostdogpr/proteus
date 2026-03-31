package proteus

import java.util.concurrent.TimeUnit

import io.grpc.Metadata
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import ox.{inScopeRunner, supervised}
import ox.flow.Flow
import zio.test.*

import proteus.GrpcTestUtils.*
import proteus.client.OxClientBackend
import proteus.server.{OxServerBackend, RequestResponseMetadata, ServerService}

object OxBackendSpec extends ZIOSpecDefault {

  def clientStreamingOx(flow: Flow[StreamRequest]): StreamResponse = {
    var sum = 0
    flow.runForeach(req => sum += req.value)
    StreamResponse(sum)
  }

  def serverStreamingOx(req: StreamRequest): Flow[StreamResponse] =
    Flow.fromIterable((1 to req.value).map(i => StreamResponse(i)))

  def bidiStreamingOx(flow: Flow[StreamRequest]): Flow[StreamResponse] =
    flow.map(req => StreamResponse(req.value * 2))

  def spec = suite("OxBackendSpec")(
    test("should discover services via gRPC reflection") {
      supervised {
        val backend       = OxServerBackend(inScopeRunner())
        val serverService = ServerService(using backend)
          .rpc(complexRpc, processComplexRequest)
          .build(testService)

        val result = testReflection(8000, serverService)
        assertTrue(result)
      }
    },
    test("should handle complex gRPC request/response with ox backend") {
      supervised {
        val backend       = OxServerBackend(inScopeRunner())
        val serverService = ServerService(using backend)
          .rpc(complexRpc, processComplexRequest)
          .build(testService)

        val port    = 8001
        val server  = NettyServerBuilder.forPort(port).addService(serverService).build().start()
        val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

        try {
          val clientBackend = new OxClientBackend(channel)
          val client1       = clientBackend.client(complexRpc, testService)
          val client2       = clientBackend.client(complexRpc, testService)
          val client3       = clientBackend.client(complexRpc, testService)

          val response1 = client1(sampleRequest)
          val response2 = client2(sampleRequest.copy(contact = ContactMethod.Phone("555-0123", "US"), priority = Priority.Low))
          val response3 = client3(sampleRequest.copy(contact = ContactMethod.Slack("my-workspace", "#general"), count = None))

          assertTrue(validateComplexResponse(response1, response2, response3))
        } finally {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        }
      }
    },
    test("should handle client and server metadata") {
      supervised {
        val backend               = OxServerBackend(inScopeRunner())
        val metadataServerService = ServerService(using backend)
          .rpcWithContext(metadataRpc, processWithMetadata)
          .build(metadataService)

        val port    = 8002
        val server  = NettyServerBuilder.forPort(port).addService(metadataServerService).build().start()
        val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

        try {
          val clientBackend = new OxClientBackend(channel)

          val requestMetadata = new Metadata()
          requestMetadata.put(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER), "ox-client-101")
          requestMetadata.put(Metadata.Key.of("user-agent", Metadata.ASCII_STRING_MARSHALLER), "grpc-ox/1.0")

          val client                       = clientBackend.clientWithMetadata(metadataRpc, metadataService)
          val (response, responseMetadata) = client(MetadataRequest("hello ox metadata"), requestMetadata)

          assertTrue(validateMetadataResponse(response, responseMetadata, "ox-client-101", "hello ox metadata"))
        } finally {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        }
      }
    },
    test("should handle client streaming") {
      supervised {
        val backend                = OxServerBackend(inScopeRunner())
        val clientStreamingService = Service("ClientStreamingService").rpc(clientStreamingRpc)
        val streamingServerService = ServerService(using backend)
          .rpc(clientStreamingRpc, clientStreamingOx)
          .build(clientStreamingService)

        val port    = 8003
        val server  = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
        val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

        try {
          val clientBackend = new OxClientBackend(channel)
          val client        = clientBackend.client(clientStreamingRpc, clientStreamingService)
          val requestFlow   = Flow.fromIterable(List(StreamRequest(1), StreamRequest(2), StreamRequest(3), StreamRequest(4)))
          val response      = client(requestFlow)

          assertTrue(response.result == 10)
        } finally {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        }
      }
    },
    test("should handle server streaming") {
      supervised {
        val backend                = OxServerBackend(inScopeRunner())
        val serverStreamingService = Service("ServerStreamingService").rpc(serverStreamingRpc)
        val streamingServerService = ServerService(using backend)
          .rpc(serverStreamingRpc, serverStreamingOx)
          .build(serverStreamingService)

        val port    = 8004
        val server  = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
        val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

        try {
          val clientBackend = new OxClientBackend(channel)
          val client        = clientBackend.client(serverStreamingRpc, serverStreamingService)
          val responseFlow  = client(StreamRequest(5))
          val responses     = responseFlow.runToList()

          assertTrue(responses == List(StreamResponse(1), StreamResponse(2), StreamResponse(3), StreamResponse(4), StreamResponse(5)))
        } finally {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        }
      }
    },
    test("should handle bidirectional streaming") {
      supervised {
        val backend                = OxServerBackend(inScopeRunner())
        val bidiStreamingService   = Service("BidiStreamingService").rpc(bidiStreamingRpc)
        val streamingServerService = ServerService(using backend)
          .rpc(bidiStreamingRpc, bidiStreamingOx)
          .build(bidiStreamingService)

        val port    = 8005
        val server  = NettyServerBuilder.forPort(port).addService(streamingServerService).build().start()
        val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

        try {
          val clientBackend = new OxClientBackend(channel)
          val client        = clientBackend.client(bidiStreamingRpc, bidiStreamingService)
          val requestFlow   = Flow.fromIterable(List(StreamRequest(10), StreamRequest(20), StreamRequest(30)))
          val responseFlow  = client(requestFlow)
          val responses     = responseFlow.runToList()

          assertTrue(responses == List(StreamResponse(20), StreamResponse(40), StreamResponse(60)))
        } finally {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
        }
      }
    }
  )
}
