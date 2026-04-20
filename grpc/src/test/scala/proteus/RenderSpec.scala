package proteus

import zio.blocks.schema.Schema
import zio.test.*

import proteus.GrpcTestUtils.*

object RenderSpec extends ZIOSpecDefault {

  case class SharedMessage(value: String) derives ProtobufCodec
  case class UnusedMessage(data: String) derives ProtobufCodec
  case class RequestWithShared(id: Int, shared: SharedMessage) derives ProtobufCodec
  case class ResponseWithShared(result: String, shared: SharedMessage) derives ProtobufCodec

  given ProtobufDeriver = ProtobufDeriver

  val sharedRpc         = Rpc.unary[RequestWithShared, ResponseWithShared]("ProcessShared")
  val serviceWithShared = Service("test.package", "ServiceWithShared").rpc(sharedRpc)

  val sharedDep = Dependency("shared").add[SharedMessage]
  val unusedDep = Dependency("unused").add[UnusedMessage]

  val options = List(
    ProtoIR.TopLevelOption("java_package", "com.test"),
    ProtoIR.TopLevelOption("csharp_namespace", "Test")
  )

  def spec = suite("RenderSpec")(
    suite("Service Rendering")(
      test("should only include used dependencies in rendered proto") {
        val renderedProto = serviceWithShared.dependsOn(sharedDep).dependsOn(unusedDep).render(options)
        val expected      = """syntax = "proto3";

package test.package;

option java_package = "com.test";
option csharp_namespace = "Test";

import "shared.proto";

service ServiceWithShared {
    rpc ProcessShared (RequestWithShared) returns (ResponseWithShared) {}
}

message RequestWithShared {
    int32 id = 1;
    SharedMessage shared = 2;
}

message ResponseWithShared {
    string result = 1;
    SharedMessage shared = 2;
}
"""

        assertTrue(renderedProto == expected)
      },
      test("should include all dependencies when all are used") {
        val priorityDep = Dependency("priority").add[Priority]
        val contactDep  = Dependency("contact").add[ContactMethod]
        val addressDep  = Dependency("address").add[Address]

        val renderedProto = testService.dependsOn(priorityDep).dependsOn(contactDep).dependsOn(addressDep).render(options)
        val expected      = """syntax = "proto3";

package test.package;

option java_package = "com.test";
option csharp_namespace = "Test";

import "priority.proto";

import "contact.proto";

import "address.proto";

service TestService {
    rpc ProcessComplex (ComplexRequest) returns (ComplexResponse) {}
}

message ComplexRequest {
    int64 id = 1;
    string name = 2;
    bool is_active = 3;
    double score = 4;
    repeated int32 numbers = 5;
    repeated string tags = 6;
    map<string, int32> metadata = 7;
    Priority priority = 8;
    ContactMethod contact = 9;
    Address address = 10;
    optional int32 count = 11;
}

message ComplexResponse {
    int64 processed_id = 1;
    string processed_name = 2;
    bool was_active = 3;
    double adjusted_score = 4;
    repeated int32 processed_numbers = 5;
    repeated string processed_tags = 6;
    map<string, int32> response_metadata = 7;
    Priority result_priority = 8;
    ContactMethod preferred_contact = 9;
    Address confirmed_address = 10;
    string processing_note = 11;
    int64 timestamp = 12;
}
"""

        assertTrue(renderedProto == expected)
      },
      test("should not include any imports when no dependencies are used") {
        val unusedDep1 = Dependency("unused1").add[UnusedMessage]
        val unusedDep2 = Dependency("unused2").add[SharedMessage]

        val renderedProto = metadataService.dependsOn(unusedDep1).dependsOn(unusedDep2).render(options)
        val expected      = """syntax = "proto3";

package test.package;

option java_package = "com.test";
option csharp_namespace = "Test";

service MetadataService {
    rpc ProcessWithMetadata (MetadataRequest) returns (MetadataResponse) {}
}

message MetadataRequest {
    string message = 1;
}

message MetadataResponse {
    string echo = 1;
    string client_id = 2;
    string server_note = 3;
}
"""

        assertTrue(renderedProto == expected)
      },
      test("should include package name and options in rendered proto") {
        val renderedProto = testService.render(options)
        val expected      = """syntax = "proto3";

package test.package;

option java_package = "com.test";
option csharp_namespace = "Test";

service TestService {
    rpc ProcessComplex (ComplexRequest) returns (ComplexResponse) {}
}

message ComplexRequest {
    int64 id = 1;
    string name = 2;
    bool is_active = 3;
    double score = 4;
    repeated int32 numbers = 5;
    repeated string tags = 6;
    map<string, int32> metadata = 7;
    Priority priority = 8;
    ContactMethod contact = 9;
    Address address = 10;
    optional int32 count = 11;
}

enum Priority {
    LOW = 0;
    MEDIUM = 1;
    HIGH = 2;
    CRITICAL = 3;
}

message ContactMethod {
    oneof value {
        Email email = 1;
        Phone phone = 2;
        Slack slack = 3;
    }
}

message Email {
    string address = 1;
}

message Phone {
    string number = 1;
    string country = 2;
}

message Slack {
    string workspace = 1;
    string channel = 2;
}

message Address {
    string street = 1;
    string city = 2;
    string country = 3;
    int32 zip_code = 4;
}

message ComplexResponse {
    int64 processed_id = 1;
    string processed_name = 2;
    bool was_active = 3;
    double adjusted_score = 4;
    repeated int32 processed_numbers = 5;
    repeated string processed_tags = 6;
    map<string, int32> response_metadata = 7;
    Priority result_priority = 8;
    ContactMethod preferred_contact = 9;
    Address confirmed_address = 10;
    string processing_note = 11;
    int64 timestamp = 12;
}
"""

        assertTrue(renderedProto == expected)
      },
      test("should handle complex nested type references") {
        val complexDep = Dependency("complex_types")
          .add[Priority]
          .add[ContactMethod]
          .add[Address]

        val renderedProto = testService.dependsOn(complexDep).render(options)
        val expected      = """syntax = "proto3";

package test.package;

option java_package = "com.test";
option csharp_namespace = "Test";

import "complex_types.proto";

service TestService {
    rpc ProcessComplex (ComplexRequest) returns (ComplexResponse) {}
}

message ComplexRequest {
    int64 id = 1;
    string name = 2;
    bool is_active = 3;
    double score = 4;
    repeated int32 numbers = 5;
    repeated string tags = 6;
    map<string, int32> metadata = 7;
    Priority priority = 8;
    ContactMethod contact = 9;
    Address address = 10;
    optional int32 count = 11;
}

message ComplexResponse {
    int64 processed_id = 1;
    string processed_name = 2;
    bool was_active = 3;
    double adjusted_score = 4;
    repeated int32 processed_numbers = 5;
    repeated string processed_tags = 6;
    map<string, int32> response_metadata = 7;
    Priority result_priority = 8;
    ContactMethod preferred_contact = 9;
    Address confirmed_address = 10;
    string processing_note = 11;
    int64 timestamp = 12;
}
"""

        assertTrue(renderedProto == expected)
      },
      test("should handle streaming services with type dependencies") {
        val streamDep = Dependency("stream_types").add[StreamRequest].add[StreamResponse]

        val renderedProto = streamingService.dependsOn(streamDep).render(options)
        val expected      = """syntax = "proto3";

package test.package;

option java_package = "com.test";
option csharp_namespace = "Test";

import "stream_types.proto";

service StreamingService {
    rpc ClientStreaming (stream StreamRequest) returns (StreamResponse) {}
    rpc ServerStreaming (StreamRequest) returns (stream StreamResponse) {}
    rpc BidiStreaming (stream StreamRequest) returns (stream StreamResponse) {}
}
"""

        assertTrue(renderedProto == expected)
      },
      test("should work with service dependency filtering") {
        val priorityDep = Dependency("priority").add[Priority]
        val addressDep  = Dependency("address").add[Address]
        val unusedDep   = Dependency("unused").add[UnusedMessage]

        val service         = Service("TestService").rpc(Rpc.unary[ComplexRequest, ComplexResponse]("Test"))
        val serviceRendered = service.dependsOn(priorityDep).dependsOn(addressDep).dependsOn(unusedDep).render(options)
        val expected        = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

import "priority.proto";

import "address.proto";

service TestService {
    rpc Test (ComplexRequest) returns (ComplexResponse) {}
}

message ComplexRequest {
    int64 id = 1;
    string name = 2;
    bool is_active = 3;
    double score = 4;
    repeated int32 numbers = 5;
    repeated string tags = 6;
    map<string, int32> metadata = 7;
    Priority priority = 8;
    ContactMethod contact = 9;
    Address address = 10;
    optional int32 count = 11;
}

message ContactMethod {
    oneof value {
        Email email = 1;
        Phone phone = 2;
        Slack slack = 3;
    }
}

message Email {
    string address = 1;
}

message Phone {
    string number = 1;
    string country = 2;
}

message Slack {
    string workspace = 1;
    string channel = 2;
}

message ComplexResponse {
    int64 processed_id = 1;
    string processed_name = 2;
    bool was_active = 3;
    double adjusted_score = 4;
    repeated int32 processed_numbers = 5;
    repeated string processed_tags = 6;
    map<string, int32> response_metadata = 7;
    Priority result_priority = 8;
    ContactMethod preferred_contact = 9;
    Address confirmed_address = 10;
    string processing_note = 11;
    int64 timestamp = 12;
}
"""

        assertTrue(serviceRendered == expected)
      },
      test("should handle simple service with minimal dependencies") {
        val sharedDep = Dependency("shared").add[SharedMessage]
        val unusedDep = Dependency("unused").add[UnusedMessage]

        val service         = Service("SimpleService").rpc(Rpc.unary[RequestWithShared, ResponseWithShared]("Process"))
        val serviceRendered = service.dependsOn(sharedDep).dependsOn(unusedDep).render(options)
        val expected        = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

import "shared.proto";

service SimpleService {
    rpc Process (RequestWithShared) returns (ResponseWithShared) {}
}

message RequestWithShared {
    int32 id = 1;
    SharedMessage shared = 2;
}

message ResponseWithShared {
    string result = 1;
    SharedMessage shared = 2;
}
"""

        assertTrue(serviceRendered == expected)
      },
      test("should render RPC comments") {
        case class CommentRequest(message: String) derives ProtobufCodec
        case class CommentResponse(echo: String) derives ProtobufCodec

        val commentedRpc       = Rpc.unary[CommentRequest, CommentResponse]("ProcessWithComment", "This RPC processes requests with special handling")
        val serviceWithComment = Service("test.package", "CommentService").rpc(commentedRpc)

        val renderedProto = serviceWithComment.render(options)
        val expected      = """syntax = "proto3";

package test.package;

option java_package = "com.test";
option csharp_namespace = "Test";

service CommentService {
    // This RPC processes requests with special handling
    rpc ProcessWithComment (CommentRequest) returns (CommentResponse) {}
}

message CommentRequest {
    string message = 1;
}

message CommentResponse {
    string echo = 1;
}
"""

        assertTrue(renderedProto == expected)
      },
      test("should render service comments") {
        case class ServiceRequest(data: String) derives ProtobufCodec
        case class ServiceResponse(result: String) derives ProtobufCodec

        val testRpc            = Rpc.unary[ServiceRequest, ServiceResponse]("ProcessData")
        val serviceWithComment = Service("test.package", "CommentedService", "This service handles data processing operations")
          .rpc(testRpc)

        val renderedProto = serviceWithComment.render(options)
        val expected      = """syntax = "proto3";

package test.package;

option java_package = "com.test";
option csharp_namespace = "Test";

// This service handles data processing operations
service CommentedService {
    rpc ProcessData (ServiceRequest) returns (ServiceResponse) {}
}

message ServiceRequest {
    string data = 1;
}

message ServiceResponse {
    string result = 1;
}
"""

        assertTrue(renderedProto == expected)
      },
      test("should detect conflicts when same type name has different definitions in service dependencies") {
        case class DifferentUser1(id: Int, email: String) derives Schema
        case class DifferentUser2(id: Int, age: Int) derives Schema

        case class Request(user1: DifferentUser1, user2: DifferentUser2) derives Schema
        case class Response() derives Schema

        val deriver = ProtobufDeriver.modifier[DifferentUser1](Modifiers.rename("User")).modifier[DifferentUser2](Modifiers.rename("User"))

        given ProtobufCodec[Request]  = Schema[Request].derive(deriver)
        given ProtobufCodec[Response] = Schema[Response].derive(deriver)

        val rpc     = Rpc.unary[Request, Response]("TestMethod")
        val service = Service("TestService").rpc(rpc)

        val error =
          try {
            service.render(options)
            ""
          } catch {
            case e: Exception => e.getMessage
          }

        assertTrue(error.contains("Conflicts found in service TestService")) &&
          assertTrue(error.contains("Type `User` is defined in different ways"))
      },
      test("nestedIn modifier resolves across request and response codecs") {
        case class CrossInnerC(value: String) derives Schema
        case class CrossRequest(c: CrossInnerC) derives Schema
        case class CrossResponse(ok: Boolean) derives Schema

        given ProtobufDeriver              = ProtobufDeriver.modifier[CrossInnerC](Modifiers.nestedIn[CrossResponse])
        given ProtobufCodec[CrossRequest]  = Schema[CrossRequest].derive(summon[ProtobufDeriver])
        given ProtobufCodec[CrossResponse] = Schema[CrossResponse].derive(summon[ProtobufDeriver])

        val rpc           = Rpc.unary[CrossRequest, CrossResponse]("Cross")
        val service       = Service("test.package", "CrossService").rpc(rpc)
        val renderedProto = service.render(options)
        val expected      = """syntax = "proto3";

package test.package;

option java_package = "com.test";
option csharp_namespace = "Test";

service CrossService {
    rpc Cross (CrossRequest) returns (CrossResponse) {}
}

message CrossRequest {
    CrossResponse.CrossInnerC c = 1;
}

message CrossResponse {
    message CrossInnerC {
        string value = 1;
    }

    bool ok = 1;
}
"""

        assertTrue(renderedProto == expected)
      },
      test("nestedIn modifier resolves when parent type appears in multiple RPCs") {
        case class PerkDiff(perkId: Int) derives Schema
        case class Session(stageId: Int) derives Schema
        case class StartRequest(session: Session) derives Schema
        case class StartResponse(ok: Boolean) derives Schema
        case class EndRequest(session: Session, diff: PerkDiff) derives Schema
        case class EndResponse(ok: Boolean) derives Schema

        given ProtobufDeriver              = ProtobufDeriver.modifier[PerkDiff](Modifiers.nestedIn[Session])
        given ProtobufCodec[StartRequest]  = Schema[StartRequest].derive(summon[ProtobufDeriver])
        given ProtobufCodec[StartResponse] = Schema[StartResponse].derive(summon[ProtobufDeriver])
        given ProtobufCodec[EndRequest]    = Schema[EndRequest].derive(summon[ProtobufDeriver])
        given ProtobufCodec[EndResponse]   = Schema[EndResponse].derive(summon[ProtobufDeriver])

        val rpc1          = Rpc.unary[StartRequest, StartResponse]("Start")
        val rpc2          = Rpc.unary[EndRequest, EndResponse]("End")
        val service       = Service("test.package", "GameService").rpc(rpc1).rpc(rpc2)
        val renderedProto = service.render(options)

        assertTrue(renderedProto.contains("Session.PerkDiff diff = 2;")) &&
          assertTrue(renderedProto.contains("message Session {\n    message PerkDiff {"))
      },
      test("nestedIn qualifier works when parent type is in a dependency") {
        case class DepPerkDiff(perkId: Int) derives Schema
        case class DepSession(stageId: Int) derives Schema
        case class DepEndRequest(session: DepSession, diff: DepPerkDiff) derives Schema
        case class DepEndResponse(ok: Boolean) derives Schema

        given ProtobufDeriver               = ProtobufDeriver.modifier[DepPerkDiff](Modifiers.nestedIn[DepSession])
        given ProtobufCodec[DepSession]     = Schema[DepSession].derive(summon[ProtobufDeriver])
        given ProtobufCodec[DepPerkDiff]    = Schema[DepPerkDiff].derive(summon[ProtobufDeriver])
        given ProtobufCodec[DepEndRequest]  = Schema[DepEndRequest].derive(summon[ProtobufDeriver])
        given ProtobufCodec[DepEndResponse] = Schema[DepEndResponse].derive(summon[ProtobufDeriver])

        val dep           = Dependency("shared_types", "test.package").add[DepSession].add[DepPerkDiff]
        val rpc           = Rpc.unary[DepEndRequest, DepEndResponse]("End")
        val service       = Service("test.package", "GameService").rpc(rpc).dependsOn(dep)
        val renderedProto = service.render(options)

        assertTrue(renderedProto.contains("DepSession.DepPerkDiff diff = 2;"))
      },
      test("nestedIn modifier leaves type at top level when target is unreachable at service level") {
        case class OrphanInner(value: String) derives Schema
        case class OrphanRequest(inner: OrphanInner) derives Schema
        case class OrphanResponse(ok: Boolean) derives Schema
        case class OrphanAncestor(x: Int) derives Schema

        given ProtobufDeriver               = ProtobufDeriver.modifier[OrphanInner](Modifiers.nestedIn[OrphanAncestor])
        given ProtobufCodec[OrphanRequest]  = Schema[OrphanRequest].derive(summon[ProtobufDeriver])
        given ProtobufCodec[OrphanResponse] = Schema[OrphanResponse].derive(summon[ProtobufDeriver])

        val rpc           = Rpc.unary[OrphanRequest, OrphanResponse]("Orphan")
        val service       = Service("test.package", "OrphanService").rpc(rpc)
        val renderedProto = service.render(options)

        assertTrue(renderedProto.contains("message OrphanInner {")) &&
          assertTrue(renderedProto.contains("OrphanInner inner = 1;"))
      }
    ),
    suite("Dependency Rendering")(
      test("should render dependency with simple types") {
        val simpleDep = Dependency("simple", "test.package").add[SharedMessage]
        val rendered  = simpleDep.render(options)
        val expected  = """syntax = "proto3";

package test.package;

option java_package = "com.test";
option csharp_namespace = "Test";

message SharedMessage {
    string value = 1;
}
"""

        assertTrue(rendered == expected)
      },
      test("should render dependency with enum types") {
        val enumDep  = Dependency("enums").add[Priority]
        val rendered = enumDep.render(options)
        val expected = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

enum Priority {
    LOW = 0;
    MEDIUM = 1;
    HIGH = 2;
    CRITICAL = 3;
}
"""

        assertTrue(rendered == expected)
      },
      test("should render dependency with complex nested types") {
        val complexDep = Dependency("complex")
          .add[Address]
          .add[ContactMethod]
        val rendered   = complexDep.render(options)
        val expected   = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

message Address {
    string street = 1;
    string city = 2;
    string country = 3;
    int32 zip_code = 4;
}

message ContactMethod {
    oneof value {
        Email email = 1;
        Phone phone = 2;
        Slack slack = 3;
    }
}

message Email {
    string address = 1;
}

message Phone {
    string number = 1;
    string country = 2;
}

message Slack {
    string workspace = 1;
    string channel = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("should filter out unused sub-dependencies when rendering") {
        val baseDep   = Dependency("base").add[SharedMessage]
        val unusedDep = Dependency("unused").add[UnusedMessage]

        val mainDep = Dependency("main")
          .add[RequestWithShared]
          .dependsOn(baseDep)
          .dependsOn(unusedDep)

        val rendered = mainDep.render(options)
        val expected = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

import "base.proto";

message RequestWithShared {
    int32 id = 1;
    SharedMessage shared = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("should include all used sub-dependencies when rendering") {
        val priorityDep = Dependency("priority").add[Priority]
        val addressDep  = Dependency("address").add[Address]
        val contactDep  = Dependency("contact").add[ContactMethod]

        val mainDep = Dependency("main")
          .add[ComplexRequest]
          .dependsOn(priorityDep)
          .dependsOn(addressDep)
          .dependsOn(contactDep)

        val rendered = mainDep.render(options)
        val expected = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

import "priority.proto";

import "address.proto";

import "contact.proto";

message ComplexRequest {
    int64 id = 1;
    string name = 2;
    bool is_active = 3;
    double score = 4;
    repeated int32 numbers = 5;
    repeated string tags = 6;
    map<string, int32> metadata = 7;
    Priority priority = 8;
    ContactMethod contact = 9;
    Address address = 10;
    optional int32 count = 11;
}
"""

        assertTrue(rendered == expected)
      },
      test("should render empty dependency without types") {
        val emptyDep = Dependency("empty", "test.package")
        val rendered = emptyDep.render(options)
        val expected = """syntax = "proto3";

package test.package;

option java_package = "com.test";
option csharp_namespace = "Test";

"""

        assertTrue(rendered == expected)
      },
      test("should handle dependency chains correctly") {
        val level1 = Dependency("level1").add[SharedMessage]
        val level2 = Dependency("level2").add[RequestWithShared].dependsOn(level1)
        val level3 = Dependency("level3").add[ResponseWithShared].dependsOn(level1)

        val rendered = level3.render(options)
        val expected = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

import "level1.proto";

message ResponseWithShared {
    string result = 1;
    SharedMessage shared = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("should handle transitive dependencies correctly") {
        val level1 = Dependency("level1").add[SharedMessage]
        val level2 = Dependency("level2").add[RequestWithShared].dependsOn(level1)
        val level3 = Dependency("level3").add[ResponseWithShared].dependsOn(level2)

        val rendered = level3.render(options)
        val expected = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

import "level1.proto";

message ResponseWithShared {
    string result = 1;
    SharedMessage shared = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("should sort types by order of declaration, then top-down in rendered output") {
        val multiTypeDep = Dependency("multi")
          .add[Priority]
          .add[ContactMethod]
          .add[Address]

        val rendered = multiTypeDep.render(options)
        val expected = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

enum Priority {
    LOW = 0;
    MEDIUM = 1;
    HIGH = 2;
    CRITICAL = 3;
}

message ContactMethod {
    oneof value {
        Email email = 1;
        Phone phone = 2;
        Slack slack = 3;
    }
}

message Email {
    string address = 1;
}

message Phone {
    string number = 1;
    string country = 2;
}

message Slack {
    string workspace = 1;
    string channel = 2;
}

message Address {
    string street = 1;
    string city = 2;
    string country = 3;
    int32 zip_code = 4;
}
"""

        assertTrue(rendered == expected)
      },
      test("should extract common types from services using fromServices") {
        val service1 = Service("Service1").rpc(Rpc.unary[ComplexRequest, ComplexResponse]("Method1"))
        val service2 = Service("Service2").rpc(Rpc.unary[ComplexRequest, ComplexResponse]("Method2"))

        val dependency = Dependency.fromServices("common.proto", service1, service2)
        val rendered   = dependency.render(options)
        val expected   = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

enum Priority {
    LOW = 0;
    MEDIUM = 1;
    HIGH = 2;
    CRITICAL = 3;
}

message ContactMethod {
    oneof value {
        Email email = 1;
        Phone phone = 2;
        Slack slack = 3;
    }
}

message Email {
    string address = 1;
}

message Phone {
    string number = 1;
    string country = 2;
}

message Slack {
    string workspace = 1;
    string channel = 2;
}

message Address {
    string street = 1;
    string city = 2;
    string country = 3;
    int32 zip_code = 4;
}
"""

        assertTrue(rendered == expected)
      },
      test("should exclude request/response types from fromServices") {
        val testRpc     = Rpc.unary[MetadataRequest, MetadataResponse]("TestMethod")
        val testService = Service("TestService").rpc(testRpc)

        val dependency = Dependency.fromServices("shared.proto", testService)
        val rendered   = dependency.render(options)
        val expected   = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

"""

        assertTrue(rendered == expected)
      },
      test("should handle multiple services with shared enums using fromServices") {
        case class RequestWithPriority(priority: Priority, data: String) derives ProtobufCodec
        case class ResponseWithPriority(priority: Priority, result: String) derives ProtobufCodec

        val rpc1     = Rpc.unary[RequestWithPriority, ResponseWithPriority]("Method1")
        val rpc2     = Rpc.unary[ComplexRequest, ComplexResponse]("Method2")
        val service1 = Service("Service1").rpc(rpc1)
        val service2 = Service("Service2").rpc(rpc2)

        val dependency = Dependency.fromServices("shared.proto", service1, service2)
        val rendered   = dependency.render(options)
        val expected   = """syntax = "proto3";

option java_package = "com.test";
option csharp_namespace = "Test";

enum Priority {
    LOW = 0;
    MEDIUM = 1;
    HIGH = 2;
    CRITICAL = 3;
}

message ContactMethod {
    oneof value {
        Email email = 1;
        Phone phone = 2;
        Slack slack = 3;
    }
}

message Email {
    string address = 1;
}

message Phone {
    string number = 1;
    string country = 2;
}

message Slack {
    string workspace = 1;
    string channel = 2;
}

message Address {
    string street = 1;
    string city = 2;
    string country = 3;
    int32 zip_code = 4;
}
"""

        assertTrue(rendered == expected)
      },
      test("should detect conflicts when same type name has different definitions in dependencies") {
        case class Status1(active: Boolean) derives Schema
        case class Status2(code: Int, message: String) derives Schema

        val deriver1 = ProtobufDeriver.modifier[Status1](Modifiers.rename("Status"))
        val deriver2 = ProtobufDeriver.modifier[Status2](Modifiers.rename("Status"))

        given ProtobufCodec[Status1] = Schema[Status1].derive(deriver1)
        given ProtobufCodec[Status2] = Schema[Status2].derive(deriver2)

        val dep = Dependency("dep").add[Status1].add[Status2]

        val error =
          try {
            dep.render(options)
            ""
          } catch {
            case e: Exception => e.getMessage
          }

        assertTrue(error.contains("Conflicts found in dependency dep")) &&
          assertTrue(error.contains("Type `Status` is defined in different ways"))
      },
      test("should not detect conflicts when 2 types have the same name but compatible definitions") {
        case class Status1(active: Boolean) derives Schema
        case class Status2(active: Boolean) derives Schema

        val deriver1 = ProtobufDeriver.modifier[Status1](Modifiers.rename("Status"))
        val deriver2 = ProtobufDeriver.modifier[Status2](Modifiers.rename("Status"))

        given ProtobufCodec[Status1] = Schema[Status1].derive(deriver1)
        given ProtobufCodec[Status2] = Schema[Status2].derive(deriver2)

        val dep = Dependency("dep").add[Status1].add[Status2]

        val error =
          try {
            dep.render(options)
            ""
          } catch {
            case e: Exception => e.getMessage
          }

        assertTrue(error == "")
      }
    )
  )
}
