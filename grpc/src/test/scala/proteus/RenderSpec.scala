package proteus

import zio.blocks.schema.Schema
import zio.test.*

import proteus.GrpcTestUtils.*

object RenderSpec extends ZIOSpecDefault {

  case class SharedMessage(value: String) derives Schema, ProtobufCodec
  case class UnusedMessage(data: String) derives Schema, ProtobufCodec
  case class RequestWithShared(id: Int, shared: SharedMessage) derives Schema, ProtobufCodec
  case class ResponseWithShared(result: String, shared: SharedMessage) derives Schema, ProtobufCodec

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
        case class CommentRequest(message: String) derives Schema, ProtobufCodec
        case class CommentResponse(echo: String) derives Schema, ProtobufCodec

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
        case class ServiceRequest(data: String) derives Schema, ProtobufCodec
        case class ServiceResponse(result: String) derives Schema, ProtobufCodec

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
        case class RequestWithPriority(priority: Priority, data: String) derives Schema, ProtobufCodec
        case class ResponseWithPriority(priority: Priority, result: String) derives Schema, ProtobufCodec

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
      }
    )
  )
}
