package proteus

import java.util.concurrent.{CompletableFuture, TimeUnit}

import scala.jdk.CollectionConverters.*

import io.grpc.Metadata
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.protobuf.services.ProtoReflectionServiceV1
import io.grpc.reflection.v1.{ServerReflectionGrpc, ServerReflectionRequest}
import zio.blocks.schema.Schema

import proteus.server.RequestResponseMetadata

object GrpcTestUtils {

  // =============================================================================
  // Complex Message Types (for testing all protobuf features)
  // =============================================================================

  enum Priority derives Schema {
    case Low, Medium, High, Critical
  }

  enum ContactMethod derives Schema {
    case Email(address: String)
    case Phone(number: String, country: String)
    case Slack(workspace: String, channel: String)
  }

  case class Address(street: String, city: String, country: String, zipCode: Int) derives Schema

  case class ComplexRequest(
    id: Long,
    name: String,
    isActive: Boolean,
    score: Double,
    numbers: List[Int],
    tags: List[String],
    metadata: Map[String, Int],
    priority: Priority,
    contact: ContactMethod,
    address: Address,
    count: Option[Int]
  ) derives Schema

  case class ComplexResponse(
    processedId: Long,
    processedName: String,
    wasActive: Boolean,
    adjustedScore: Double,
    processedNumbers: List[Int],
    processedTags: List[String],
    responseMetadata: Map[String, Int],
    resultPriority: Priority,
    preferredContact: ContactMethod,
    confirmedAddress: Address,
    processingNote: String,
    timestamp: Long
  ) derives Schema

  // =============================================================================
  // Simple Metadata Types (for testing client/server metadata)
  // =============================================================================

  case class MetadataRequest(message: String) derives Schema
  case class MetadataResponse(echo: String, clientId: String, serverNote: String) derives Schema

  // =============================================================================
  // Simple Streaming Types (for testing all streaming patterns)
  // =============================================================================

  case class StreamRequest(value: Int) derives Schema
  case class StreamResponse(result: Int) derives Schema

  // =============================================================================
  // RPC Definitions
  // =============================================================================

  given ProtobufDeriver = ProtobufDeriver

  // Unary RPC with complex messages
  val complexRpc  = Rpc.unary[ComplexRequest, ComplexResponse]("ProcessComplex")
  val testService = Service("test.package", "TestService").rpc(complexRpc)

  // Unary RPC with metadata
  val metadataRpc     = Rpc.unary[MetadataRequest, MetadataResponse]("ProcessWithMetadata")
  val metadataService = Service("test.package", "MetadataService").rpc(metadataRpc)

  // Streaming RPCs
  val clientStreamingRpc = Rpc.clientStreaming[StreamRequest, StreamResponse]("ClientStreaming")
  val serverStreamingRpc = Rpc.serverStreaming[StreamRequest, StreamResponse]("ServerStreaming")
  val bidiStreamingRpc   = Rpc.bidiStreaming[StreamRequest, StreamResponse]("BidiStreaming")

  val streamingService = Service("test.package", "StreamingService")
    .rpc(clientStreamingRpc)
    .rpc(serverStreamingRpc)
    .rpc(bidiStreamingRpc)

  // =============================================================================
  // Test Data
  // =============================================================================

  val sampleRequest = ComplexRequest(
    12345L,
    "test-service",
    true,
    95.5,
    List(1, 2, 3, 5, 8, 13),
    List("important", "test", "grpc"),
    Map("version" -> 1, "env" -> 2),
    Priority.High,
    ContactMethod.Email("test@example.com"),
    Address("123 Test St", "Test City", "Test Country", 12345),
    Some(42)
  )

  // =============================================================================
  // Server Logic (shared implementations for testing)
  // =============================================================================

  def processComplexRequest(req: ComplexRequest): ComplexResponse =
    ComplexResponse(
      processedId = req.id * 2,
      processedName = req.name.toUpperCase,
      wasActive = req.isActive,
      adjustedScore = req.score + 10.0,
      processedNumbers = req.numbers.map(_ + 1),
      processedTags = req.tags.map(_.reverse),
      responseMetadata = req.metadata ++ Map("processed" -> 1, "server" -> 2),
      resultPriority = req.priority match {
        case Priority.Low      => Priority.Medium
        case Priority.Medium   => Priority.High
        case Priority.High     => Priority.Critical
        case Priority.Critical => Priority.Critical
      },
      preferredContact = req.contact,
      confirmedAddress = req.address.copy(zipCode = req.address.zipCode + 1000),
      processingNote = req.count.map(c => s"Count was $c").getOrElse("No count provided"),
      timestamp = java.lang.System.currentTimeMillis()
    )

  def processWithMetadata(req: MetadataRequest, ctx: RequestResponseMetadata): MetadataResponse = {
    val clientId = Option(ctx.requestMetadata.get(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER))).getOrElse("unknown")
    ctx.responseMetadata.put(Metadata.Key.of("server-response", Metadata.ASCII_STRING_MARSHALLER), "processed")
    MetadataResponse(req.message.toUpperCase, clientId, "Server processed with metadata")
  }

  // =============================================================================
  // Validation Functions
  // =============================================================================

  def validateComplexResponse(response: ComplexResponse, response2: ComplexResponse, response3: ComplexResponse): Boolean =
    response.processedId == 24690L &&
      response.processedName == "TEST-SERVICE" &&
      response.wasActive == true &&
      response.adjustedScore == 105.5 &&
      response.responseMetadata.contains("processed") &&
      response.responseMetadata("processed") == 1 &&
      response.responseMetadata("server") == 2 &&
      response.processedNumbers == List(2, 3, 4, 6, 9, 14) &&
      response.processedTags == List("tnatropmi", "tset", "cprg") &&
      response.resultPriority == Priority.Critical &&
      response.preferredContact == ContactMethod.Email("test@example.com") &&
      response.confirmedAddress.street == "123 Test St" &&
      response.confirmedAddress.zipCode == 13345 &&
      response.processingNote == "Count was 42" &&
      response2.resultPriority == Priority.Medium &&
      response2.preferredContact == ContactMethod.Phone("555-0123", "US") &&
      response3.preferredContact == ContactMethod.Slack("my-workspace", "#general") &&
      response3.processingNote == "No count provided"

  def validateMetadataResponse(response: MetadataResponse, responseMetadata: Metadata, expectedClientId: String, expectedMessage: String): Boolean =
    response.echo == expectedMessage.toUpperCase &&
      response.clientId == expectedClientId &&
      response.serverNote == "Server processed with metadata" &&
      responseMetadata.get(Metadata.Key.of("server-response", Metadata.ASCII_STRING_MARSHALLER)) == "processed"

  // =============================================================================
  // Test Utilities
  // =============================================================================

  def testReflection(port: Int, serverDefinition: io.grpc.ServerServiceDefinition): Boolean = {
    val reflectionService = ProtoReflectionServiceV1.newInstance
    val server            = NettyServerBuilder.forPort(port).addService(serverDefinition).addService(reflectionService).build().start()
    val channel           = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

    val reflectionStub  = ServerReflectionGrpc.newStub(channel)
    val future          = new CompletableFuture[List[String]]()
    val requestObserver = reflectionStub.serverReflectionInfo(new io.grpc.stub.StreamObserver[io.grpc.reflection.v1.ServerReflectionResponse] {
      def onNext(response: io.grpc.reflection.v1.ServerReflectionResponse): Unit =
        if (response.hasListServicesResponse) future.complete(response.getListServicesResponse.getServiceList.asScala.map(_.getName).toList): Unit
      def onError(throwable: Throwable): Unit                                    = future.completeExceptionally(throwable): Unit
      def onCompleted(): Unit                                                    = if (!future.isDone) future.complete(List.empty): Unit
    })
    requestObserver.onNext(ServerReflectionRequest.newBuilder().setListServices("").build())
    requestObserver.onCompleted()
    val serviceNames    = future.get(2, TimeUnit.SECONDS)

    server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
    channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)

    serviceNames.contains("test.package.TestService")
  }
}
