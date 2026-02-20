package proteus

import zio.blocks.schema.*
import zio.blocks.schema.Schema
import zio.test.*

import proteus.Modifiers.*
import proteus.ProtoIR.{CompilationUnit, Statement}
import proteus.internal.Renderer

object ProtobufRenderSpec extends ZIOSpecDefault {

  val deriver                    = ProtobufDeriver
  val deriverWithOptionalAsOneOf = deriver.enable(ProtobufDeriver.DerivationFlag.OptionalAsOneOf)
  val deriverWithAutoPrefixEnums = deriver.enable(ProtobufDeriver.DerivationFlag.AutoPrefixEnums)
  val deriverWithNestedOneOf     = deriver.enable(ProtobufDeriver.DerivationFlag.NestedOneOf)

  def renderCodec[A](codec: ProtobufCodec[A], packageName: String = "test"): String = {
    val topLevelDefs    = ProtobufCodec.toProtoIR(codec)
    val statements      = topLevelDefs.map(Statement.TopLevelStatement(_)).distinct
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
      test("map field with nested message value renders correctly") {
        case class Inner(value: String) derives Schema
        case class MapMessage(id: Int, data: Map[String, Inner]) derives Schema
        val codec    = Schema[MapMessage].derive(deriver.modifier[Inner](nested))
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message MapMessage {
    message Inner {
        string value = 1;
    }

    int32 id = 1;
    map<string, Inner> data = 2;
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
    message KeyMessageIntEntry {
        KeyMessage key = 1;
        int32 value = 2;
    }

    int32 id = 1;
    repeated KeyMessageIntEntry data = 2;
}

message KeyMessage {
    string value = 1;
}
"""

        assertTrue(rendered == expected)
      },
      test("map field with optional value should render correctly") {
        case class MapWithOptionalValue(map: Map[Int, Option[Int]]) derives Schema
        val codec    = Schema[MapWithOptionalValue].derive(deriver)
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message MapWithOptionalValue {
    message IntOptionIntEntry {
        int32 key = 1;
        optional int32 value = 2;
    }

    repeated IntOptionIntEntry map = 1;
}
"""
        assertTrue(rendered == expected)
      }
    ),
    suite("Modifier Rendering")(
      test("proteus nested modifier creates nested message") {
        case class NestedData(value: String) derives Schema
        case class MessageWithNested(id: Int, data: NestedData) derives Schema

        val codec    = Schema[MessageWithNested].derive(deriver.modifier[NestedData](nested))
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
      test("proteus oneof modifier forces oneOf instead of enum") {
        enum ForceOneOf derives Schema { case First, Second, Third }
        case class MessageWithOneOf(choice: ForceOneOf) derives Schema

        val codec    = Schema[MessageWithOneOf].derive(deriver.modifier[ForceOneOf](oneOf))
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
      test("proteus oneof inline modifier inlines variant into parent message") {
        enum InlineContact derives Schema {
          case Email(address: String)
          case Phone(number: String)
        }
        case class MessageWithInline(contact: InlineContact) derives Schema

        val codec    = Schema[MessageWithInline].derive(deriver.modifier[InlineContact](oneOf(OneOfFlag.Inline)))
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
      test("proteus oneof inline modifier with transform") {
        enum ContactType derives Schema {
          case Email(address: String)
          case Phone(number: String)
        }

        case class ContactWrapper(contact: ContactType) derives Schema
        case class MessageWithContact(id: Int, contact: ContactWrapper) derives Schema

        val transformedCodec: ProtobufCodec[ContactWrapper] =
          Schema[ContactType]
            .derive(deriver.modifier[ContactType](oneOf(OneOfFlag.Inline)))
            .transform[ContactWrapper](contact => ContactWrapper(contact), wrapper => wrapper.contact)

        val parentCodec = Schema[MessageWithContact].derive(deriver.instance(transformedCodec))

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
      test("proteus oneof nested modifier creates nested messages in oneOf") {
        enum NestedContact derives Schema {
          case Email(address: String)
          case Phone(number: String, country: String)
        }
        case class ContactMessage(contact: NestedContact) derives Schema

        val codec    = Schema[ContactMessage].derive(deriver.modifier[NestedContact](oneOf(OneOfFlag.Nested)))
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message ContactMessage {
    NestedContact contact = 1;
}

message NestedContact {
    message Email {
        string address = 1;
    }

    message Phone {
        string number = 1;
        string country = 2;
    }

    oneof value {
        Email email = 1;
        Phone phone = 2;
    }
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus oneof nested vs regular oneof behavior") {
        enum RegularContact derives Schema {
          case Email(address: String)
          case Phone(number: String)
        }

        enum NestedContact derives Schema {
          case Email(address: String)
          case Phone(number: String)
        }

        case class RegularMessage(contact: RegularContact) derives Schema
        case class NestedMessage(contact: NestedContact) derives Schema

        val regularCodec    = Schema[RegularMessage].derive(deriver)
        val nestedCodec     = Schema[NestedMessage].derive(deriver.modifier[NestedContact](oneOf(OneOfFlag.Nested)))
        val regularRendered = renderCodec(regularCodec)
        val nestedRendered  = renderCodec(nestedCodec)

        val expectedRegular = """syntax = "proto3";

package test;

message RegularMessage {
    RegularContact contact = 1;
}

message RegularContact {
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
}
"""

        val expectedNested = """syntax = "proto3";

package test;

message NestedMessage {
    NestedContact contact = 1;
}

message NestedContact {
    message Email {
        string address = 1;
    }

    message Phone {
        string number = 1;
    }

    oneof value {
        Email email = 1;
        Phone phone = 2;
    }
}
"""

        assertTrue(regularRendered == expectedRegular) &&
          assertTrue(nestedRendered == expectedNested)
      },
      test("proteus reserved modifier skips reserved field numbers") {
        case class ReservedMessage(id: Int, name: String, value: String, active: Boolean) derives Schema

        val codec    = Schema[ReservedMessage].derive(deriver.modifier[ReservedMessage](reserved(2, 4, 6)))
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
      test("proteus reserved modifier forces a field number") {
        case class ReservedMessage(id: Int, name: String, value: String, active: Boolean) derives Schema

        val codec    = Schema[ReservedMessage].derive(deriver.modifier[ReservedMessage]("name", reserved(6)))
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message ReservedMessage {
    int32 id = 1;
    string value = 2;
    bool active = 3;
    string name = 6;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus reserved modifier forces a enum case number") {
        enum ReservedEnum derives Schema { case Name, Value, Active }

        val codec    = Schema[ReservedEnum].derive(deriver.modifier[ReservedEnum]("Value", reserved(6)))
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

enum ReservedEnum {
    NAME = 0;
    ACTIVE = 1;
    VALUE = 6;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus reserved modifier forces a oneof field number") {
        enum ReservedOneOf derives Schema {
          case Name(name: String)
          case Value(value: String)
          case Active(value: String)
        }

        val codec    =
          Schema[ReservedOneOf].derive(deriver.modifier[ReservedOneOf]("Value", reserved(6)))
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message ReservedOneOf {
    oneof value {
        Name name = 1;
        Active active = 2;
        Value value = 6;
    }
}

message Name {
    string name = 1;
}

message Value {
    string value = 1;
}

message Active {
    string value = 1;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus reserved modifier on inline oneOf field controls case indexes") {
        enum ContactType derives Schema {
          case Email(address: String)
          case Phone(number: String)
        }
        case class ContactMessage(
          id: Int,
          contact: ContactType,
          contact2: ContactType,
          test: String
        ) derives Schema

        val codec    = Schema[ContactMessage].derive(
          deriver
            .modifier[ContactType](oneOf(OneOfFlag.Inline))
            .modifier[ContactMessage]("contact", reserved(2, 5))
            .modifier[ContactMessage]("contact2", reserved(3, 6))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message ContactMessage {
    int32 id = 1;
    oneof contact {
        Email email = 2;
        Phone phone = 5;
    }
    oneof contact2 {
        Email email = 3;
        Phone phone = 6;
    }
    string test = 4;
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
      test("proteus rename modifier renames type") {
        case class User(id: Int, name: String) derives Schema
        case class UserMessage(user: User) derives Schema

        val codec    = Schema[UserMessage].derive(deriver.modifier[User](rename("RenamedUser")))
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
      test("proteus rename modifier renames record fields") {
        case class FieldRenameTest(id: Int, name: String, active: Boolean) derives Schema

        val codec    = Schema[FieldRenameTest].derive(
          deriver
            .modifier[FieldRenameTest]("id", rename("identifier"))
            .modifier[FieldRenameTest]("name", rename("full_name"))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message FieldRenameTest {
    int32 identifier = 1;
    string full_name = 2;
    bool active = 3;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus rename modifier renames enum members") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class StatusRenameTest(status: Status) derives Schema

        val codec    = Schema[StatusRenameTest].derive(
          deriver
            .modifier[Status]("Active", rename("ACTIVE_STATUS"))
            .modifier[Status]("Inactive", rename("INACTIVE_STATUS"))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message StatusRenameTest {
    Status status = 1;
}

enum Status {
    ACTIVE_STATUS = 0;
    INACTIVE_STATUS = 1;
    PENDING = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus enum prefix modifier adds prefix to enum members") {
        enum Priority derives Schema { case Low, Medium, High }
        case class PriorityMessage(priority: Priority) derives Schema

        val codec    = Schema[PriorityMessage].derive(deriver.modifier[Priority](enumPrefix("STATUS")))
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
      test("proteus excluded modifier excludes field from rendered proto") {
        case class MessageWithExcluded(id: Int, name: String, excluded: String) derives Schema
        val codec    = Schema[MessageWithExcluded].derive(deriver.modifier[MessageWithExcluded]("excluded", excluded))
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
      test("proteus excluded modifier excludes enum cases from rendered proto") {
        enum StatusWithExcluded derives Schema {
          case Active
          case Inactive
          case Pending
        }
        case class StatusMessage(status: StatusWithExcluded) derives Schema
        val codec    = Schema[StatusMessage].derive(deriver.modifier[StatusWithExcluded]("Inactive", excluded))
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
      test("proteus excluded modifier excludes variant cases from rendered proto") {
        enum ContactWithExcluded derives Schema {
          case Email(address: String)
          case Phone(number: String)
          case Slack(workspace: String)
        }
        case class ContactMessage(contact: ContactWithExcluded) derives Schema
        val codec    = Schema[ContactMessage].derive(deriver.modifier[ContactWithExcluded]("Phone", excluded))
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
      },
      test("proteus comment modifier renders field comments") {
        case class MessageWithComments(id: Int, name: String, email: String) derives Schema

        val codec    = Schema[MessageWithComments].derive(
          deriver
            .modifier[MessageWithComments]("id", comment("User identifier"))
            .modifier[MessageWithComments]("name", comment("User's display name"))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message MessageWithComments {
    int32 id = 1; // User identifier
    string name = 2; // User's display name
    string email = 3;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus comment modifier renders type-level comments") {
        case class UserProfile(
          id: Int,
          name: String
        ) derives Schema

        enum UserStatus derives Schema {
          case Active, Inactive, Suspended
        }
        case class MessageWithEnums(profile: UserProfile, status: UserStatus) derives Schema

        val codec    = Schema[MessageWithEnums].derive(
          deriver
            .modifier[UserProfile](comment("User profile information"))
            .modifier[UserStatus](comment("Status levels for users"))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message MessageWithEnums {
    UserProfile profile = 1;
    UserStatus status = 2;
}

// User profile information
message UserProfile {
    int32 id = 1;
    string name = 2;
}

// Status levels for users
enum UserStatus {
    ACTIVE = 0;
    INACTIVE = 1;
    SUSPENDED = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus comment modifier renders both type-level and field-level comments") {
        case class FullUser(id: Int, name: String, email: String) derives Schema

        val codec    = Schema[FullUser].derive(
          deriver
            .modifier[FullUser](comment("Complete user information"))
            .modifier[FullUser]("id", comment("Unique user identifier"))
            .modifier[FullUser]("name", comment("Display name"))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

// Complete user information
message FullUser {
    int32 id = 1; // Unique user identifier
    string name = 2; // Display name
    string email = 3;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus comment modifier renders oneof comments") {
        enum ContactMethod derives Schema {
          case Email(address: String)
          case Phone(number: String)
        }
        case class ContactMessage(contact: ContactMethod) derives Schema

        val codec    = Schema[ContactMessage].derive(
          deriver
            .modifier[ContactMethod](oneOf(OneOfFlag.Inline))
            .modifier[ContactMethod](comment("Preferred contact method"))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message ContactMessage {
    // Preferred contact method
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
      test("proteus comment modifier renders multiline type-level comments") {
        case class UserData(id: Int, name: String) derives Schema

        val codec    = Schema[UserData].derive(
          deriver.modifier[UserData](comment("User data structure\nContains basic user information\nUsed for authentication"))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

// User data structure
// Contains basic user information
// Used for authentication
message UserData {
    int32 id = 1;
    string name = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus comment modifier renders multiline field comments") {
        case class DataMessage(id: Int, description: String) derives Schema

        val codec    = Schema[DataMessage].derive(
          deriver
            .modifier[DataMessage]("description", comment("Long description field\nCan contain multiple lines\nMay include detailed information"))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message DataMessage {
    int32 id = 1;
    // Long description field
    // Can contain multiple lines
    // May include detailed information
    string description = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus comment modifier renders multiline enum comments") {
        enum Priority derives Schema { case Low, Medium, High }
        case class Task(priority: Priority) derives Schema

        val codec    = Schema[Task].derive(
          deriver.modifier[Priority](comment("Task priority levels\nUsed to determine urgency\nAffects processing order"))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message Task {
    Priority priority = 1;
}

// Task priority levels
// Used to determine urgency
// Affects processing order
enum Priority {
    LOW = 0;
    MEDIUM = 1;
    HIGH = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus comment modifier renders single-line enum element comments") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class StatusMessage(status: Status) derives Schema

        val codec    = Schema[StatusMessage].derive(
          deriver
            .modifier[Status]("Active", comment("Currently active"))
            .modifier[Status]("Inactive", comment("Temporarily disabled"))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message StatusMessage {
    Status status = 1;
}

enum Status {
    ACTIVE = 0; // Currently active
    INACTIVE = 1; // Temporarily disabled
    PENDING = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus comment modifier renders multiline enum element comments") {
        enum Priority derives Schema { case Low, Medium, High }
        case class Task(priority: Priority) derives Schema

        val codec    = Schema[Task].derive(
          deriver
            .modifier[Priority]("Low", comment("Low priority task\nProcessed when resources available\nNot urgent"))
            .modifier[Priority]("High", comment("High priority task\nProcessed immediately"))
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message Task {
    Priority priority = 1;
}

enum Priority {
    // Low priority task
    // Processed when resources available
    // Not urgent
    LOW = 0;
    MEDIUM = 1;
    // High priority task
    // Processed immediately
    HIGH = 2;
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
      },
      test("NestedOneOf flag creates nested messages for enum variants") {
        enum Contact derives Schema {
          case Email(address: String)
          case Phone(number: String, country: String)
        }
        case class ContactMessage(contact: Contact) derives Schema

        val standardCodec = Schema[ContactMessage].derive(deriver)
        val nestedCodec   = Schema[ContactMessage].derive(deriverWithNestedOneOf)

        val standardRendered = renderCodec(standardCodec)
        val nestedRendered   = renderCodec(nestedCodec)

        val expectedStandard = """syntax = "proto3";

package test;

message ContactMessage {
    Contact contact = 1;
}

message Contact {
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

        val expectedNested = """syntax = "proto3";

package test;

message ContactMessage {
    Contact contact = 1;
}

message Contact {
    message Email {
        string address = 1;
    }

    message Phone {
        string number = 1;
        string country = 2;
    }

    oneof value {
        Email email = 1;
        Phone phone = 2;
    }
}
"""

        assertTrue(standardRendered == expectedStandard) &&
          assertTrue(nestedRendered == expectedNested)
      },
      test("NestedOneOf flag creates nested messages for enum variants except when unnested modifier is used") {
        enum Contact derives Schema {
          case Email(address: String)
          case Phone(number: String, country: String)
        }
        object Contact              {
          given Schema[Email] = Schema.derived
          given Schema[Phone] = Schema.derived
        }
        case class ContactMessage(contact: Contact) derives Schema

        val nestedCodec    = Schema[ContactMessage].derive(deriverWithNestedOneOf.modifier[Contact.Email](unnested))
        val nestedRendered = renderCodec(nestedCodec)

        val expectedNested = """syntax = "proto3";

package test;

message ContactMessage {
    Contact contact = 1;
}

message Contact {
    message Phone {
        string number = 1;
        string country = 2;
    }

    oneof value {
        Email email = 1;
        Phone phone = 2;
    }
}

message Email {
    string address = 1;
}
"""

        assertTrue(nestedRendered == expectedNested)
      },
      test("AutoPrefixEnums flag adds type name as prefix to enum members") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class StatusMessage(status: Status) derives Schema

        val standardCodec   = Schema[StatusMessage].derive(deriver)
        val autoPrefixCodec = Schema[StatusMessage].derive(deriverWithAutoPrefixEnums)

        val standardRendered   = renderCodec(standardCodec)
        val autoPrefixRendered = renderCodec(autoPrefixCodec)

        val expectedStandard = """syntax = "proto3";

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

        val expectedAutoPrefix = """syntax = "proto3";

package test;

message StatusMessage {
    Status status = 1;
}

enum Status {
    STATUS_ACTIVE = 0;
    STATUS_INACTIVE = 1;
    STATUS_PENDING = 2;
}
"""

        assertTrue(standardRendered == expectedStandard) &&
          assertTrue(autoPrefixRendered == expectedAutoPrefix)
      },
      test("AutoPrefixEnums handles complex type names correctly") {
        enum XMLParser derives Schema   { case Ready     }
        enum HTTPStatus derives Schema  { case Active    }
        enum HTML5Parser derives Schema { case Valid     }
        enum IOUtils derives Schema     { case Available }

        case class ComplexMessage(
          xml: XMLParser,
          http: HTTPStatus,
          html: HTML5Parser,
          io: IOUtils
        ) derives Schema

        val codec    = Schema[ComplexMessage].derive(deriverWithAutoPrefixEnums)
        val rendered = renderCodec(codec)

        val expected = """syntax = "proto3";

package test;

message ComplexMessage {
    XMLParser xml = 1;
    HTTPStatus http = 2;
    HTML5Parser html = 3;
    IOUtils io = 4;
}

enum XMLParser {
    XML_PARSER_READY = 0;
}

enum HTTPStatus {
    HTTP_STATUS_ACTIVE = 0;
}

enum HTML5Parser {
    HTML5_PARSER_VALID = 0;
}

enum IOUtils {
    IO_UTILS_AVAILABLE = 0;
}
"""

        assertTrue(rendered == expected)
      },
      test("proteus enum suffix modifier adds suffix to enum members") {
        enum Priority derives Schema { case Low, Medium, High }
        case class PriorityMessage(priority: Priority) derives Schema

        val codec    = Schema[PriorityMessage].derive(deriver.modifier[Priority](enumSuffix("_LEVEL")))
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message PriorityMessage {
    Priority priority = 1;
}

enum Priority {
    LOW__LEVEL = 0;
    MEDIUM__LEVEL = 1;
    HIGH__LEVEL = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("enumSuffix modifier works with AutoPrefixEnums flag") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class StatusMessage(status: Status) derives Schema

        val codecWithExplicitSuffix = Schema[StatusMessage].derive(
          deriverWithAutoPrefixEnums.modifier[Status](enumSuffix("_STATE"))
        )

        val rendered = renderCodec(codecWithExplicitSuffix)
        val expected = """syntax = "proto3";

package test;

message StatusMessage {
    Status status = 1;
}

enum Status {
    STATUS_ACTIVE__STATE = 0;
    STATUS_INACTIVE__STATE = 1;
    STATUS_PENDING__STATE = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("enumPrefix modifier takes priority over AutoPrefixEnums flag") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class StatusMessage(status: Status) derives Schema

        val codecWithExplicitPrefix = Schema[StatusMessage].derive(
          deriverWithAutoPrefixEnums.modifier[Status](enumPrefix("USER"))
        )

        val rendered = renderCodec(codecWithExplicitPrefix)
        val expected = """syntax = "proto3";

package test;

message StatusMessage {
    Status status = 1;
}

enum Status {
    USER_ACTIVE = 0;
    USER_INACTIVE = 1;
    USER_PENDING = 2;
}
"""

        assertTrue(rendered == expected)
      },
      test("enum prefix and suffix can be combined") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class StatusMessage(status: Status) derives Schema

        val codecWithBoth = Schema[StatusMessage].derive(
          deriver
            .modifier[Status](enumPrefix("USER"))
            .modifier[Status](enumSuffix("_FLAG"))
        )

        val rendered = renderCodec(codecWithBoth)
        val expected = """syntax = "proto3";

package test;

message StatusMessage {
    Status status = 1;
}

enum Status {
    USER_ACTIVE__FLAG = 0;
    USER_INACTIVE__FLAG = 1;
    USER_PENDING__FLAG = 2;
}
"""

        assertTrue(rendered == expected)
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
    ),
    suite("Field-Specific Instance Override")(
      test("field-specific instance renders custom type for overridden field") {
        case class TimestampWrapper(millis: Long) derives Schema

        val timestampCodec: ProtobufCodec[Long] =
          Schema[TimestampWrapper]
            .derive(deriver)
            .transform[Long](_.millis, TimestampWrapper(_))

        case class Event(id: Int, createdAt: Long) derives Schema

        val codec    = Schema[Event].derive(deriver.instance[Event, Long]("createdAt", timestampCodec))
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message Event {
    int32 id = 1;
    TimestampWrapper created_at = 2;
}

message TimestampWrapper {
    int64 millis = 1;
}
"""

        assertTrue(rendered == expected)
      },
      test("field-specific instance only affects specified field in rendering") {
        case class TimestampWrapper(millis: Long) derives Schema

        val timestampCodec: ProtobufCodec[Long] =
          Schema[TimestampWrapper]
            .derive(deriver)
            .transform[Long](_.millis, TimestampWrapper(_))

        case class Event(id: Int, createdAt: Long, updatedAt: Long) derives Schema

        val codec    = Schema[Event].derive(deriver.instance[Event, Long]("createdAt", timestampCodec))
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message Event {
    int32 id = 1;
    TimestampWrapper created_at = 2;
    int64 updated_at = 3;
}

message TimestampWrapper {
    int64 millis = 1;
}
"""

        assertTrue(rendered == expected)
      },
      test("multiple field-specific instances render correctly") {
        case class MillisWrapper(millis: Long) derives Schema
        case class SecondsWrapper(seconds: Long) derives Schema

        val millisCodec: ProtobufCodec[Long] =
          Schema[MillisWrapper].derive(deriver).transform[Long](_.millis, MillisWrapper(_))

        val secondsCodec: ProtobufCodec[Long] =
          Schema[SecondsWrapper].derive(deriver).transform[Long](_.seconds, SecondsWrapper(_))

        case class Event(id: Int, createdAt: Long, updatedAt: Long) derives Schema

        val codec    = Schema[Event].derive(
          deriver
            .instance[Event, Long]("createdAt", millisCodec)
            .instance[Event, Long]("updatedAt", secondsCodec)
        )
        val rendered = renderCodec(codec)
        val expected = """syntax = "proto3";

package test;

message Event {
    int32 id = 1;
    MillisWrapper created_at = 2;
    SecondsWrapper updated_at = 3;
}

message MillisWrapper {
    int64 millis = 1;
}

message SecondsWrapper {
    int64 seconds = 1;
}
"""

        assertTrue(rendered == expected)
      }
    )
  )
}
