package proteus

import zio.blocks.schema.*
import zio.blocks.schema.Modifier.config
import zio.blocks.schema.Schema
import zio.test.*

import proteus.ProtoIR.{CompilationUnit, Statement}

object ProtobufRenderSpec extends ZIOSpecDefault {

  val deriver                    = ProtobufDeriver
  val deriverWithOptionalAsOneOf = deriver.enable(ProtobufDeriver.DerivationFlag.OptionalAsOneOf)

  def renderCodec[A](codec: ProtobufCodec[A], packageName: String = "test"): String = {
    val topLevelDefs    = ProtobufCodec.toProtoIR(codec)
    val statements      = topLevelDefs.map(Statement.TopLevelStatement(_))
    val compilationUnit = CompilationUnit(Some(packageName), statements, List.empty)
    Renderer.render(compilationUnit)
  }

  def spec = suite("ProtobufRenderSpec")(
    suite("Basic Rendering")(
      test("simple message renders correctly") {
        case class SimpleMessage(id: Int, name: String, active: Boolean) derives Schema
        val codec    = Schema[SimpleMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message SimpleMessage {
    int32 id = 1;
    string name = 2;
    bool active = 3;
}
"""

        assertTrue(rendered == expected)
      },
      test("enum renders correctly") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class StatusMessage(status: Status) derives Schema
        val codec    = Schema[StatusMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message StatusMessage {
    Status status = 1;
}

enum Status {
    ACTIVE = 0;
    INACTIVE = 1;
    PENDING = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("optional field renders correctly") {
        case class OptionalMessage(id: Int, value: Option[String]) derives Schema
        val codec    = Schema[OptionalMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message OptionalMessage {
    int32 id = 1;
    optional string value = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("nested message renders correctly") {
        case class Inner(value: String) derives Schema
        case class Outer(id: Int, inner: Inner) derives Schema
        val codec    = Schema[Outer].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message Outer {
    int32 id = 1;
    Inner inner = 2;
}

message Inner {
    string value = 1;
}
"""

        assertTrue(rendered == expected)
      },
      test("oneof variant renders correctly") {
        enum ContactMethod derives Schema {
          case Email(address: String)
          case Phone(number: String, country: String)
        }
        case class ContactMessage(contact: ContactMethod) derives Schema
        val codec    = Schema[ContactMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message ContactMessage {
    ContactMethod contact = 1;
}

message ContactMethod {
    oneof value {
        Email email = 1;
        Phone phone = 2;
    }
}

message Email {
    string address = 1;
}

message Phone {
    string number = 1;
    string country = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("bytes primitive renders correctly") {
        case class BytesMessage(id: Int, data: Array[Byte], optionalData: Option[Array[Byte]]) derives Schema
        val codec    = Schema[BytesMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message BytesMessage {
    int32 id = 1;
    bytes data = 2;
    optional bytes optional_data = 3;
}
"""

        assertTrue(rendered == expected)
      },
      test("map field with primitive key renders correctly") {
        case class MapMessage(id: Int, data: Map[String, Int]) derives Schema
        val codec    = Schema[MapMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message MapMessage {
    int32 id = 1;
    map<string, int32> data = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("map field with message key renders correctly") {
        case class KeyMessage(value: String) derives Schema
        case class MapMessage(id: Int, data: Map[KeyMessage, Int]) derives Schema
        val codec    = Schema[MapMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message MapMessage {
    int32 id = 1;
    repeated KeyMessageIntEntry data = 2;
}

message KeyMessageIntEntry {
    KeyMessage key = 1;
    int32 value = 2;
}

message KeyMessage {
    string value = 1;
}
"""

        assertTrue(rendered == expected)
      }
    ),
    suite("Modifier Rendering")(
      test("proteus.nested modifier creates nested message") {
        @config("proteus.nested", "true")
        case class NestedData(value: String) derives Schema
        case class MessageWithNested(id: Int, data: NestedData) derives Schema

        val codec    = Schema[MessageWithNested].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message MessageWithNested {
    message NestedData {
        string value = 1;
    }
    
    int32 id = 1;
    NestedData data = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus.oneof modifier forces oneOf instead of enum") {
        @config("proteus.oneof", "true")
        enum ForceOneOf derives Schema { case First, Second, Third }
        case class MessageWithOneOf(choice: ForceOneOf) derives Schema

        val codec    = Schema[MessageWithOneOf].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message MessageWithOneOf {
    ForceOneOf choice = 1;
}

message ForceOneOf {
    oneof value {
        First first = 1;
        Second second = 2;
        Third third = 3;
    }
}

message First {}

message Second {}

message Third {}
"""

        assertTrue(rendered == expected)
      },
      test("proteus.inline modifier inlines variant into parent message") {
        @config("proteus.inline", "true")
        enum InlineContact derives Schema {
          case Email(address: String)
          case Phone(number: String)
        }
        case class MessageWithInline(contact: InlineContact) derives Schema

        val codec    = Schema[MessageWithInline].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message MessageWithInline {
    oneof contact {
        Email email = 1;
        Phone phone = 2;
    }
}

message Email {
    string address = 1;
}

message Phone {
    string number = 1;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus.inline modifier with transform") {
        @config("proteus.inline", "true")
        enum ContactType derives Schema {
          case Email(address: String)
          case Phone(number: String)
        }

        case class ContactWrapper(contact: ContactType) derives Schema
        case class MessageWithContact(id: Int, contact: ContactWrapper) derives Schema

        val transformedCodec: ProtobufCodec[ContactWrapper] =
          Schema[ContactType]
            .derive(deriver)
            .transform[ContactWrapper](contact => ContactWrapper(contact), wrapper => wrapper.contact)

        val parentCodec = Schema[MessageWithContact]
          .deriving(deriver)
          .instance(Schema[ContactWrapper].reflect.asRecord.get.typeName, transformedCodec)
          .derive

        val rendered = renderCodec(parentCodec)

        val expected = """syntax = "proto3";

package test;

message MessageWithContact {
    int32 id = 1;
    oneof contact {
        Email email = 2;
        Phone phone = 3;
    }
}

message Email {
    string address = 1;
}

message Phone {
    string number = 1;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus.reserved modifier skips reserved field numbers") {
        @config("proteus.reserved", "2,4,6")
        case class ReservedMessage(id: Int, name: String, value: String, active: Boolean) derives Schema

        val codec    = Schema[ReservedMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message ReservedMessage {
    reserved 2, 4, 6;
    
    int32 id = 1;
    string name = 3;
    string value = 5;
    bool active = 7;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus.reserved modifier on inline oneOf field controls case indexes") {
        @config("proteus.inline", "true")
        enum ContactType derives Schema {
          case Email(address: String)
          case Phone(number: String)
        }
        case class ContactMessage(
          id: Int,
          @config("proteus.reserved", "2,5") contact: ContactType,
          test: String
        ) derives Schema

        val codec    = Schema[ContactMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message ContactMessage {
    int32 id = 1;
    oneof contact {
        Email email = 2;
        Phone phone = 5;
    }
    string test = 3;
}

message Email {
    string address = 1;
}

message Phone {
    string number = 1;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus.rename modifier renames type") {
        @config("proteus.rename", "RenamedUser")
        case class User(id: Int, name: String) derives Schema
        case class UserMessage(user: User) derives Schema

        val codec    = Schema[UserMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message UserMessage {
    RenamedUser user = 1;
}

message RenamedUser {
    int32 id = 1;
    string name = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus.enum.prefix modifier adds prefix to enum members") {
        @config("proteus.enum.prefix", "STATUS")
        enum Priority derives Schema { case Low, Medium, High }
        case class PriorityMessage(priority: Priority) derives Schema

        val codec    = Schema[PriorityMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message PriorityMessage {
    Priority priority = 1;
}

enum Priority {
    STATUS_LOW = 0;
    STATUS_MEDIUM = 1;
    STATUS_HIGH = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus.exclude modifier excludes field from rendered proto") {
        case class MessageWithExcluded(
          id: Int,
          name: String,
          @config("proteus.exclude", "true") excluded: String
        ) derives Schema
        val codec    = Schema[MessageWithExcluded].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message MessageWithExcluded {
    int32 id = 1;
    string name = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus.exclude modifier excludes enum cases from rendered proto") {
        enum StatusWithExcluded derives Schema {
          case Active
          @config("proteus.exclude", "true") case Inactive
          case Pending
        }
        case class StatusMessage(status: StatusWithExcluded) derives Schema
        val codec    = Schema[StatusMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message StatusMessage {
    StatusWithExcluded status = 1;
}

enum StatusWithExcluded {
    ACTIVE = 0;
    PENDING = 1;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus.exclude modifier excludes variant cases from rendered proto") {
        enum ContactWithExcluded derives Schema {
          case Email(address: String)
          @config("proteus.exclude", "true") case Phone(number: String)
          case Slack(workspace: String)
        }
        case class ContactMessage(contact: ContactWithExcluded) derives Schema
        val codec    = Schema[ContactMessage].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message ContactMessage {
    ContactWithExcluded contact = 1;
}

message ContactWithExcluded {
    oneof value {
        Email email = 1;
        Slack slack = 2;
    }
}

message Email {
    string address = 1;
}

message Slack {
    string workspace = 1;
}
"""

        assertTrue(rendered == expected)
      }
    ),
    suite("Derivation Flags")(
      test("OptionalAsOneOf flag renders optional fields as oneOf") {
        case class StandardOptional(id: Int, value: Option[String]) derives Schema
        case class OneOfOptional(id: Int, value: Option[String]) derives Schema

        val standardCodec = Schema[StandardOptional].derive(deriver)
        val oneOfCodec    = Schema[OneOfOptional].derive(deriverWithOptionalAsOneOf)

        val standardRendered = renderCodec(standardCodec)
        val oneOfRendered    = renderCodec(oneOfCodec)

        val expectedStandard = """syntax = "proto3";

package test;

message StandardOptional {
    int32 id = 1;
    optional string value = 2;
}
"""

        val expectedOneOf = """syntax = "proto3";

package test;

message OneOfOptional {
    int32 id = 1;
    oneof value {
        Empty no_value = 2;
        string value_value = 3;
    }
}

message Empty {}
"""

        assertTrue(standardRendered == expectedStandard) &&
          assertTrue(oneOfRendered == expectedOneOf)
      }
    ),
    suite("Recursive Types")(
      test("recursive type renders correctly") {
        case class Recursive(a: Int, b: Option[Recursive]) derives Schema
        val codec    = Schema[Recursive].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message Recursive {
    int32 a = 1;
    optional Recursive b = 2;
}
"""

        assertTrue(rendered == expected)
      }
    )
  )
}
