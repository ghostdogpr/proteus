package proteus

import zio.blocks.schema.Schema
import zio.test.*

import proteus.GrpcTestUtils.*

object RenderSpec extends ZIOSpecDefault {

  case class SharedMessage(value: String) derives Schema
  case class UnusedMessage(data: String) derives Schema
  case class RequestWithShared(id: Int, shared: SharedMessage) derives Schema
  case class ResponseWithShared(result: String, shared: SharedMessage) derives Schema

  val sharedRpc         = Rpc.unary[RequestWithShared, ResponseWithShared]("ProcessShared")
  val serviceWithShared = Service("ServiceWithShared").rpc(sharedRpc)

  val sharedDep = Dependency("shared.proto").add[SharedMessage]
  val unusedDep = Dependency("unused.proto").add[UnusedMessage]

  val packageName = Some("test.package")
  val options     = List(
    ProtoIR.TopLevelOption("java_package", "com.test.proto"),
    ProtoIR.TopLevelOption("csharp_namespace", "Test.Proto")
  )

  def spec = suite("RenderSpec")(
    suite("Service Rendering")(
      test("should only include used dependencies in rendered proto") {
        val renderedProto = serviceWithShared.render(packageName, options, sharedDep, unusedDep)

        assertTrue(
          renderedProto.contains("import \"shared.proto\";") &&
            !renderedProto.contains("import \"unused.proto\";") &&
            renderedProto.contains("service ServiceWithShared") &&
            renderedProto.contains("rpc ProcessShared")
        )
      },
      test("should include all dependencies when all are used") {
        val priorityDep = Dependency("priority.proto").add[Priority]
        val contactDep  = Dependency("contact.proto").add[ContactMethod]
        val addressDep  = Dependency("address.proto").add[Address]

        val renderedProto = testService.render(packageName, options, priorityDep, contactDep, addressDep)

        assertTrue(
          renderedProto.contains("import \"priority.proto\";") &&
            renderedProto.contains("import \"contact.proto\";") &&
            renderedProto.contains("import \"address.proto\";") &&
            renderedProto.contains("service TestService") &&
            renderedProto.contains("rpc ProcessComplex")
        )
      },
      test("should not include any imports when no dependencies are used") {
        val unusedDep1 = Dependency("unused1.proto").add[UnusedMessage]
        val unusedDep2 = Dependency("unused2.proto").add[SharedMessage]

        val renderedProto = metadataService.render(packageName, options, unusedDep1, unusedDep2)

        assertTrue(
          !renderedProto.contains("import \"unused1.proto\";") &&
            !renderedProto.contains("import \"unused2.proto\";") &&
            renderedProto.contains("service MetadataService") &&
            renderedProto.contains("rpc ProcessWithMetadata")
        )
      },
      test("should include package name and options in rendered proto") {
        val renderedProto = testService.render(packageName, options)

        assertTrue(
          renderedProto.contains("package test.package;") &&
            renderedProto.contains("option java_package = com.test.proto;") &&
            renderedProto.contains("option csharp_namespace = Test.Proto;")
        )
      },
      test("should handle complex nested type references") {
        val complexDep = Dependency("complex_types.proto")
          .add[Priority]
          .add[ContactMethod]
          .add[Address]

        val renderedProto = testService.render(packageName, options, complexDep)

        assertTrue(
          renderedProto.contains("import \"complex_types.proto\";") &&
            renderedProto.contains("service TestService") &&
            !renderedProto.contains("enum Priority") && // Should be imported, not defined
            !renderedProto.contains("message Address")  // Should be imported, not defined
        )
      },
      test("should handle streaming services with type dependencies") {
        val streamDep = Dependency("stream_types.proto").add[StreamRequest].add[StreamResponse]

        val renderedProto = streamingService.render(packageName, options, streamDep)

        assertTrue(
          renderedProto.contains("import \"stream_types.proto\";") &&
            renderedProto.contains("service StreamingService") &&
            renderedProto.contains("rpc ClientStreaming") &&
            renderedProto.contains("rpc ServerStreaming") &&
            renderedProto.contains("rpc BidiStreaming")
        )
      },
      test("should work with service dependency filtering") {
        val priorityDep = Dependency("priority.proto").add[Priority]
        val addressDep  = Dependency("address.proto").add[Address]
        val unusedDep   = Dependency("unused.proto").add[UnusedMessage]

        val service         = Service("TestService").rpc(Rpc.unary[ComplexRequest, ComplexResponse]("Test"))
        val serviceRendered = service.render(packageName, options, priorityDep, addressDep, unusedDep)

        assertTrue(
          serviceRendered.contains("import \"priority.proto\";") &&
            serviceRendered.contains("import \"address.proto\";") &&
            !serviceRendered.contains("import \"unused.proto\";") && // Should be filtered out
            serviceRendered.contains("service TestService") &&
            serviceRendered.contains("rpc Test") &&
            serviceRendered.contains("message ComplexRequest") &&    // Should be defined in service
            serviceRendered.contains("message ComplexResponse")      // Should be defined in service
        )
      },
      test("should handle simple service with minimal dependencies") {
        val sharedDep = Dependency("shared.proto").add[SharedMessage]
        val unusedDep = Dependency("unused.proto").add[UnusedMessage]

        val service         = Service("SimpleService").rpc(Rpc.unary[RequestWithShared, ResponseWithShared]("Process"))
        val serviceRendered = service.render(packageName, options, sharedDep, unusedDep)

        assertTrue(
          serviceRendered.contains("import \"shared.proto\";") &&
            !serviceRendered.contains("import \"unused.proto\";") &&
            serviceRendered.contains("service SimpleService") &&
            serviceRendered.contains("rpc Process") &&
            serviceRendered.contains("message RequestWithShared") &&
            serviceRendered.contains("message ResponseWithShared")
        )
      }
    ),
    suite("Dependency Rendering")(
      test("should render dependency with simple types") {
        val simpleDep = Dependency("simple.proto").add[SharedMessage]
        val rendered  = simpleDep.render(packageName, options)

        assertTrue(
          rendered.contains("package test.package;") &&
            rendered.contains("option java_package = com.test.proto;") &&
            rendered.contains("option csharp_namespace = Test.Proto;") &&
            rendered.contains("message SharedMessage") &&
            rendered.contains("string value = 1;") &&
            !rendered.contains("import")
        )
      },
      test("should render dependency with enum types") {
        val enumDep  = Dependency("enums.proto").add[Priority]
        val rendered = enumDep.render(packageName, options)

        assertTrue(
          rendered.contains("enum Priority") &&
            rendered.contains("LOW = 0;") &&
            rendered.contains("MEDIUM = 1;") &&
            rendered.contains("HIGH = 2;") &&
            rendered.contains("CRITICAL = 3;")
        )
      },
      test("should render dependency with complex nested types") {
        val complexDep = Dependency("complex.proto")
          .add[Address]
          .add[ContactMethod]
        val rendered   = complexDep.render(packageName, options)

        assertTrue(
          rendered.contains("message Address") &&
            rendered.contains("message ContactMethod") &&
            rendered.contains("oneof value") &&
            rendered.contains("Email email = 1;") &&
            rendered.contains("Phone phone = 2;") &&
            rendered.contains("Slack slack = 3;")
        )
      },
      test("should filter out unused sub-dependencies when rendering") {
        val baseDep   = Dependency("base.proto").add[SharedMessage]
        val unusedDep = Dependency("unused.proto").add[UnusedMessage]

        val mainDep = Dependency("main.proto")
          .add[RequestWithShared]
          .dependsOn(baseDep)
          .dependsOn(unusedDep)

        val rendered = mainDep.render(packageName, options)

        assertTrue(
          rendered.contains("import \"base.proto\";") &&
            !rendered.contains("import \"unused.proto\";") &&
            rendered.contains("message RequestWithShared") &&
            rendered.contains("SharedMessage shared = 2")
        )
      },
      test("should include all used sub-dependencies when rendering") {
        val priorityDep = Dependency("priority.proto").add[Priority]
        val addressDep  = Dependency("address.proto").add[Address]
        val contactDep  = Dependency("contact.proto").add[ContactMethod]

        val mainDep = Dependency("main.proto")
          .add[ComplexRequest]
          .dependsOn(priorityDep)
          .dependsOn(addressDep)
          .dependsOn(contactDep)

        val rendered = mainDep.render(packageName, options)

        assertTrue(
          rendered.contains("import \"priority.proto\";") &&
            rendered.contains("import \"address.proto\";") &&
            rendered.contains("import \"contact.proto\";") &&
            rendered.contains("message ComplexRequest") &&
            rendered.contains("Priority priority = 8;") &&
            rendered.contains("Address address = 10")
        )
      },
      test("should render empty dependency without types") {
        val emptyDep = Dependency("empty.proto")
        val rendered = emptyDep.render(packageName, options)

        assertTrue(
          rendered.contains("package test.package;") &&
            rendered.contains("option java_package = com.test.proto;") &&
            rendered.contains("option csharp_namespace = Test.Proto;") &&
            !rendered.contains("message") &&
            !rendered.contains("enum") &&
            !rendered.contains("import")
        )
      },
      test("should handle dependency chains correctly") {
        val level1 = Dependency("level1.proto").add[SharedMessage]
        val level2 = Dependency("level2.proto").add[RequestWithShared].dependsOn(level1)
        val level3 = Dependency("level3.proto").add[ResponseWithShared].dependsOn(level1)

        val rendered = level3.render(packageName, options)

        assertTrue(
          rendered.contains("import \"level1.proto\";") &&
            rendered.contains("message ResponseWithShared")
        )
      },
      test("should sort types alphabetically in rendered output") {
        val multiTypeDep = Dependency("multi.proto")
          .add[Priority] // Enum
          .add[Address] // Message starting with A
          .add[ContactMethod] // Message starting with C

        val rendered      = multiTypeDep.render(packageName, options)
        val addressIndex  = rendered.indexOf("message Address")
        val contactIndex  = rendered.indexOf("message ContactMethod")
        val priorityIndex = rendered.indexOf("enum Priority")

        assertTrue(
          addressIndex < contactIndex &&    // Address comes before ContactMethod
            addressIndex < priorityIndex && // Address comes before Priority
            contactIndex < priorityIndex    // ContactMethod comes before Priority
        )
      },
      test("should extract common types from services using fromServices") {
        val service1 = Service("Service1").rpc(Rpc.unary[ComplexRequest, ComplexResponse]("Method1"))
        val service2 = Service("Service2").rpc(Rpc.unary[ComplexRequest, ComplexResponse]("Method2"))

        val dependency = Dependency.fromServices("common.proto", service1, service2)
        val rendered   = dependency.render(packageName, options)

        assertTrue(
          rendered.contains("enum Priority") &&
            rendered.contains("message Address") &&
            rendered.contains("message ContactMethod") &&
            !rendered.contains("message ComplexRequest") && // Should be excluded as request/response type
            !rendered.contains("message ComplexResponse")   // Should be excluded as request/response type
        )
      },
      test("should exclude request/response types from fromServices") {
        val testRpc     = Rpc.unary[MetadataRequest, MetadataResponse]("TestMethod")
        val testService = Service("TestService").rpc(testRpc)

        val dependency = Dependency.fromServices("shared.proto", testService)
        val rendered   = dependency.render(packageName, options)

        assertTrue(
          !rendered.contains("message MetadataRequest") &&
            !rendered.contains("message MetadataResponse") &&
            !rendered.contains("message")
        )
      },
      test("should handle multiple services with shared enums using fromServices") {
        case class RequestWithPriority(priority: Priority, data: String) derives Schema
        case class ResponseWithPriority(priority: Priority, result: String) derives Schema

        val rpc1     = Rpc.unary[RequestWithPriority, ResponseWithPriority]("Method1")
        val rpc2     = Rpc.unary[ComplexRequest, ComplexResponse]("Method2")
        val service1 = Service("Service1").rpc(rpc1)
        val service2 = Service("Service2").rpc(rpc2)

        val dependency = Dependency.fromServices("shared.proto", service1, service2)
        val rendered   = dependency.render(packageName, options)

        assertTrue(
          rendered.contains("enum Priority") &&
            rendered.contains("message Address") &&
            rendered.contains("message ContactMethod") &&
            !rendered.contains("message RequestWithPriority") &&
            !rendered.contains("message ResponseWithPriority") &&
            !rendered.contains("message ComplexRequest") &&
            !rendered.contains("message ComplexResponse")
        )
      }
    )
  )
}
