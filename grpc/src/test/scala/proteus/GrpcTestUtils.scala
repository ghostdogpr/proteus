package proteus

import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.jdk.CollectionConverters.*
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.protobuf.services.ProtoReflectionServiceV1
import io.grpc.reflection.v1.{ServerReflectionGrpc, ServerReflectionRequest}
import zio.blocks.schema.Schema

object GrpcTestUtils {

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

  val complexRpc = Rpc.unary[ComplexRequest, ComplexResponse]("ProcessComplex")
  val testService = Service("TestService").rpc(complexRpc)

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
        case Priority.Low => Priority.Medium
        case Priority.Medium => Priority.High
        case Priority.High => Priority.Critical
        case Priority.Critical => Priority.Critical
      },
      preferredContact = req.contact,
      confirmedAddress = req.address.copy(zipCode = req.address.zipCode + 1000),
      processingNote = req.count.map(c => s"Count was $c").getOrElse("No count provided"),
      timestamp = java.lang.System.currentTimeMillis()
    )

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

  def testReflection(port: Int, serverDefinition: io.grpc.ServerServiceDefinition): Boolean = {
    val reflectionService = ProtoReflectionServiceV1.newInstance
    val server = NettyServerBuilder.forPort(port).addService(serverDefinition).addService(reflectionService).build().start()
    val channel = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()

    val reflectionStub = ServerReflectionGrpc.newStub(channel)
    val future = new CompletableFuture[List[String]]()
    val requestObserver = reflectionStub.serverReflectionInfo(new io.grpc.stub.StreamObserver[io.grpc.reflection.v1.ServerReflectionResponse] {
      def onNext(response: io.grpc.reflection.v1.ServerReflectionResponse): Unit =
        if (response.hasListServicesResponse) future.complete(response.getListServicesResponse.getServiceList.asScala.map(_.getName).toList): Unit
      def onError(throwable: Throwable): Unit = future.completeExceptionally(throwable): Unit
      def onCompleted(): Unit = if (!future.isDone) future.complete(List.empty): Unit
    })
    requestObserver.onNext(ServerReflectionRequest.newBuilder().setListServices("").build())
    requestObserver.onCompleted()
    val serviceNames = future.get(2, TimeUnit.SECONDS)

    server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
    channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)

    serviceNames.contains("TestService")
  }
}