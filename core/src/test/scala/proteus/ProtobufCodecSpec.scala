package proteus

import java.time.*

import scala.util.Try

import zio.blocks.schema.*
import zio.blocks.schema.binding.Binding
import zio.test.*
import zio.test.Assertion.*

import proteus.Modifiers.*

object ProtobufCodecSpec extends ZIOSpecDefault {

  type DateTime = OffsetDateTime

  object DateTime {
    def unsafeSystemNow(): DateTime = OffsetDateTime.now()

    val timeZoneId: ZoneId = ZoneId.of("Asia/Seoul")
    val min: DateTime      = OffsetDateTime.ofInstant(Instant.EPOCH, timeZoneId)

    def ofEpochMilli(millis: Long, zoneId: ZoneId = timeZoneId): DateTime =
      OffsetDateTime.ofInstant(Instant.ofEpochMilli(millis), zoneId)
  }

  extension (dateTime: DateTime) {
    def toEpochMilli: Long =
      dateTime.toInstant.toEpochMilli
  }

  case class DateTimeWrapper(currentTimeMillis: Long) derives Schema

  opaque type UserId = String
  object UserId {
    def apply(value: String): UserId             = value
    extension (userId: UserId) def value: String = userId
  }

  val deriver                    = ProtobufDeriver
  val deriverWithOptionalAsOneOf = deriver.enable(ProtobufDeriver.DerivationFlag.OptionalAsOneOf)
  val deriverWithAutoPrefixEnums = deriver.enable(ProtobufDeriver.DerivationFlag.AutoPrefixEnums)

  val testDateTimeSchema: ProtobufCodec[OffsetDateTime] =
    Schema[DateTimeWrapper]
      .derive(deriver)
      .transform[OffsetDateTime](
        wrapper => {
          val millis = wrapper.currentTimeMillis
          if (millis == 0) DateTime.min else DateTime.ofEpochMilli(millis)
        },
        dt => DateTimeWrapper(dt.toEpochMilli)
      )

  def spec = suite("ProtobufCodecSpec")(
    suite("Simple Types")(
      test("message with basic primitives") {
        case class SimpleMessage(id: Int, name: String, active: Boolean) derives Schema
        val codec = Schema[SimpleMessage].derive(deriver)

        val original = SimpleMessage(42, "test", true)
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with default values") {
        case class DefaultMessage(id: Int, name: String, active: Boolean) derives Schema
        val codec = Schema[DefaultMessage].derive(deriver)

        val original = DefaultMessage(0, "", false)
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("Enum Tests")(
      test("message with enum field") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class StatusMessage(id: Int, status: Status) derives Schema
        val codec = Schema[StatusMessage].derive(deriver)

        val variants = List(Status.Active, Status.Inactive, Status.Pending)
        val results  = variants.map { status =>
          val original = StatusMessage(1, status)
          val encoded  = codec.encode(original)
          val decoded  = codec.decode(encoded)
          decoded
        }

        assert(results.map(_.status))(equalTo(variants))
      },
      test("enum as root codec encoding/decoding") {
        enum Status derives Schema { case Active, Inactive, Pending }

        // Create a direct codec for the enum (this tests the new Enum case in loop function)
        val enumCodec = Schema[Status].derive(deriver)

        val testCases = List(Status.Active, Status.Inactive, Status.Pending)

        val results = testCases.map { enumValue =>
          val encoded = enumCodec.encode(enumValue)
          val decoded = enumCodec.decode(encoded)
          decoded == enumValue
        }

        // Verify all enum values can be used as root codec
        assert(results.forall(identity))(isTrue)
      },
      test("oneOf field variants") {
        enum ContactInfo derives Schema {
          case Email(address: String)
          case Phone(number: String)
          case Social(platform: String, handle: String)
        }
        case class ContactMessage(contact: ContactInfo) derives Schema
        val codec = Schema[ContactMessage].derive(deriver)

        val variants = List(
          ContactInfo.Email("test@example.com"),
          ContactInfo.Phone("555-0123"),
          ContactInfo.Social("twitter", "@testuser")
        )

        val results = variants.map { contact =>
          val original = ContactMessage(contact)
          val encoded  = codec.encode(original)
          val decoded  = codec.decode(encoded)
          decoded
        }

        assert(results.map(_.contact))(equalTo(variants))
      }
    ),
    suite("DateTime Tests")(
      test("message with DateTime field") {
        case class TimeMessage(id: Int, timestamp: DateTime) derives Schema

        val codec = Schema[TimeMessage].deriving(deriver).instance(TypeName.offsetDateTime, testDateTimeSchema).derive

        val timestamps = List(
          DateTime.min,
          DateTime.ofEpochMilli(DateTime.unsafeSystemNow().toEpochMilli),
          DateTime.ofEpochMilli(0L),
          DateTime.ofEpochMilli(1000000000000L)
        )

        val results = timestamps.map { timestamp =>
          val original = TimeMessage(1, timestamp)
          val encoded  = codec.encode(original)
          val decoded  = codec.decode(encoded)
          decoded
        }

        assert(results.map(_.timestamp))(equalTo(timestamps))
      }
    ),
    suite("Optional Fields")(
      test("message with Option field") {
        case class OptionalMessage(id: Int, value: Option[String]) derives Schema
        val codec = Schema[OptionalMessage].derive(deriver)

        val withSome = OptionalMessage(1, Some("present"))
        val withNone = OptionalMessage(2, None)

        val encodedSome = codec.encode(withSome)
        val decodedSome = codec.decode(encodedSome)

        val encodedNone = codec.encode(withNone)
        val decodedNone = codec.decode(encodedNone)

        assert(decodedSome)(equalTo(withSome)) &&
          assert(decodedNone)(equalTo(withNone))
      },
      test("optional field None vs Some empty string encoding") {
        case class OptionalFieldMessage(id: Int, optionalValue: Option[String]) derives Schema
        val codec = Schema[OptionalFieldMessage].derive(deriver)

        val messageWithNone           = OptionalFieldMessage(1, None)
        val messageWithEmptyString    = OptionalFieldMessage(1, Some(""))
        val messageWithNonEmptyString = OptionalFieldMessage(1, Some("test"))

        val encodedNone        = codec.encode(messageWithNone)
        val encodedEmptyString = codec.encode(messageWithEmptyString)
        val encodedNonEmpty    = codec.encode(messageWithNonEmptyString)

        val decodedNone        = codec.decode(encodedNone)
        val decodedEmptyString = codec.decode(encodedEmptyString)
        val decodedNonEmpty    = codec.decode(encodedNonEmpty)

        assert(decodedNone)(equalTo(messageWithNone)) &&
          assert(decodedEmptyString)(equalTo(messageWithEmptyString)) &&
          assert(decodedNonEmpty)(equalTo(messageWithNonEmptyString)) &&
          assert(encodedNone.length)(isLessThan(encodedEmptyString.length)) &&
          assert(encodedEmptyString.length)(isLessThan(encodedNonEmpty.length))
      },
      test("optional primitives None vs Some default values") {
        case class OptionalPrimitivesMessage(
          optInt: Option[Int],
          optLong: Option[Long],
          optBool: Option[Boolean],
          optDouble: Option[Double],
          optFloat: Option[Float]
        ) derives Schema
        val codec = Schema[OptionalPrimitivesMessage].derive(deriver)

        val messageWithNone     = OptionalPrimitivesMessage(None, None, None, None, None)
        val messageWithDefaults = OptionalPrimitivesMessage(Some(0), Some(0L), Some(false), Some(0.0), Some(0.0f))

        val encodedNone     = codec.encode(messageWithNone)
        val encodedDefaults = codec.encode(messageWithDefaults)

        val decodedNone     = codec.decode(encodedNone)
        val decodedDefaults = codec.decode(encodedDefaults)

        assert(decodedNone)(equalTo(messageWithNone)) &&
          assert(decodedDefaults)(equalTo(messageWithDefaults)) &&
          assert(encodedNone.length)(isLessThan(encodedDefaults.length))
      },
      test("optional enum None vs Some default value") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class OptionalEnumMessage(id: Int, status: Option[Status]) derives Schema
        val codec = Schema[OptionalEnumMessage].derive(deriver)

        val messageWithNone    = OptionalEnumMessage(1, None)
        val messageWithDefault = OptionalEnumMessage(1, Some(Status.Active))

        val encodedNone    = codec.encode(messageWithNone)
        val encodedDefault = codec.encode(messageWithDefault)

        val decodedNone    = codec.decode(encodedNone)
        val decodedDefault = codec.decode(encodedDefault)

        assert(decodedNone)(equalTo(messageWithNone)) &&
          assert(decodedDefault)(equalTo(messageWithDefault)) &&
          assert(encodedNone.length)(isLessThan(encodedDefault.length))
      }
    ),
    suite("Optional Fields as OneOf")(
      test("message with Option field derived as OneOf") {
        case class OptionalAsOneOfMessage(id: Int, value: Option[String]) derives Schema
        val codec = Schema[OptionalAsOneOfMessage].derive(deriverWithOptionalAsOneOf)

        val withSome = OptionalAsOneOfMessage(1, Some("present"))
        val withNone = OptionalAsOneOfMessage(2, None)

        val encodedSome = codec.encode(withSome)
        val decodedSome = codec.decode(encodedSome)

        val encodedNone = codec.encode(withNone)
        val decodedNone = codec.decode(encodedNone)

        assert(decodedSome)(equalTo(withSome)) &&
          assert(decodedNone)(equalTo(withNone))
      },
      test("OneOf optional encoding vs standard optional encoding different") {
        case class StandardOptional(id: Int, value: Option[String]) derives Schema
        case class OneOfOptional(id: Int, value: Option[String]) derives Schema

        val standardCodec = Schema[StandardOptional].derive(deriver)
        val oneOfCodec    = Schema[OneOfOptional].derive(deriverWithOptionalAsOneOf)

        val messageWithSome = (StandardOptional(1, Some("test")), OneOfOptional(1, Some("test")))
        val messageWithNone = (StandardOptional(1, None), OneOfOptional(1, None))

        val standardEncodedSome = standardCodec.encode(messageWithSome._1)
        val oneOfEncodedSome    = oneOfCodec.encode(messageWithSome._2)

        val standardEncodedNone = standardCodec.encode(messageWithNone._1)
        val oneOfEncodedNone    = oneOfCodec.encode(messageWithNone._2)

        assert(standardEncodedSome)(not(equalTo(oneOfEncodedSome))) &&
          assert(standardEncodedNone)(not(equalTo(oneOfEncodedNone)))
      },
      test("multiple optional fields as OneOf") {
        case class MultipleOptionalMessage(
          id: Int,
          name: Option[String],
          count: Option[Int],
          active: Option[Boolean]
        ) derives Schema
        val codec = Schema[MultipleOptionalMessage].derive(deriverWithOptionalAsOneOf)

        val testCases = List(
          MultipleOptionalMessage(1, Some("test"), Some(42), Some(true)),
          MultipleOptionalMessage(2, None, Some(0), None),
          MultipleOptionalMessage(3, Some(""), None, Some(false)),
          MultipleOptionalMessage(4, None, None, None)
        )

        val results = testCases.map { original =>
          val encoded = codec.encode(original)
          val decoded = codec.decode(encoded)
          decoded
        }

        assert(results)(equalTo(testCases))
      },
      test("optional nested message as OneOf") {
        case class NestedMessage(value: String, count: Int) derives Schema
        case class OptionalNestedAsOneOf(id: Int, nested: Option[NestedMessage]) derives Schema
        val codec = Schema[OptionalNestedAsOneOf].derive(deriverWithOptionalAsOneOf)

        val withNested    = OptionalNestedAsOneOf(1, Some(NestedMessage("test", 42)))
        val withoutNested = OptionalNestedAsOneOf(2, None)

        val encodedWithNested = codec.encode(withNested)
        val decodedWithNested = codec.decode(encodedWithNested)

        val encodedWithoutNested = codec.encode(withoutNested)
        val decodedWithoutNested = codec.decode(encodedWithoutNested)

        assert(decodedWithNested)(equalTo(withNested)) &&
          assert(decodedWithoutNested)(equalTo(withoutNested))
      },
      test("optional enum as OneOf") {
        enum Priority derives Schema { case Low, Medium, High }
        case class OptionalEnumAsOneOf(id: Int, priority: Option[Priority]) derives Schema
        val codec = Schema[OptionalEnumAsOneOf].derive(deriverWithOptionalAsOneOf)

        val withPriority    = OptionalEnumAsOneOf(1, Some(Priority.High))
        val withoutPriority = OptionalEnumAsOneOf(2, None)

        val encodedWithPriority = codec.encode(withPriority)
        val decodedWithPriority = codec.decode(encodedWithPriority)

        val encodedWithoutPriority = codec.encode(withoutPriority)
        val decodedWithoutPriority = codec.decode(encodedWithoutPriority)

        assert(decodedWithPriority)(equalTo(withPriority)) &&
          assert(decodedWithoutPriority)(equalTo(withoutPriority))
      }
    ),
    suite("Nested Messages")(
      test("message with nested message") {
        case class Inner(value: String) derives Schema
        case class Outer(id: Int, inner: Inner) derives Schema
        val outerCodec = Schema[Outer].derive(deriver)

        val original = Outer(1, Inner("nested"))
        val encoded  = outerCodec.encode(original)
        val decoded  = outerCodec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("optional message None vs Some with default values") {
        case class InnerMessage(value: String, count: Int) derives Schema
        case class OptionalMessageMessage(id: Int, inner: Option[InnerMessage]) derives Schema
        val codec = Schema[OptionalMessageMessage].derive(deriver)

        val messageWithNone    = OptionalMessageMessage(1, None)
        val messageWithDefault = OptionalMessageMessage(1, Some(InnerMessage("", 0)))

        val encodedNone    = codec.encode(messageWithNone)
        val encodedDefault = codec.encode(messageWithDefault)

        val decodedNone    = codec.decode(encodedNone)
        val decodedDefault = codec.decode(encodedDefault)

        assert(decodedNone)(equalTo(messageWithNone)) &&
          assert(decodedDefault)(equalTo(messageWithDefault)) &&
          assert(encodedNone.length)(isLessThan(encodedDefault.length))
      }
    ),
    suite("Collections")(
      test("message with List[Int]") {
        case class NumberMessage(name: String, numbers: List[Int]) derives Schema
        val codec = Schema[NumberMessage].derive(deriver)

        val original = NumberMessage("test", List(1, 2, 3, 4, 5))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with empty List[Int]") {
        case class EmptyListMessage(name: String, numbers: List[Int]) derives Schema
        val codec = Schema[EmptyListMessage].derive(deriver)

        val original = EmptyListMessage("empty", List.empty)
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with List[Int] containing zeros") {
        case class ZeroListMessage(name: String, numbers: List[Int]) derives Schema
        val codec = Schema[ZeroListMessage].derive(deriver)

        val original = ZeroListMessage("zeros", List(0, 1, 0, 2, 0))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with List of messages") {
        case class Item(name: String, value: Int) derives Schema
        case class Container(items: List[Item]) derives Schema
        val containerCodec = Schema[Container].derive(deriver)

        val original = Container(List(Item("a", 1), Item("b", 2), Item("c", 3)))
        val encoded  = containerCodec.encode(original)
        val decoded  = containerCodec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with transformed List of messages") {
        case class MyList[A](items: List[A]) derives Schema
        given [A: Schema]: Schema[MyList[A]] = Schema[MyList[A]].wrapTotal(MyList[A](_), _.items)

        case class Item(name: String, value: Int) derives Schema
        case class Container(items: MyList[Item]) derives Schema
        val containerCodec = Schema[Container].derive(deriver)

        val original = Container(MyList(List(Item("a", 1), Item("b", 2), Item("c", 3))))
        val encoded  = containerCodec.encode(original)
        val decoded  = containerCodec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with List of transformed oneofs") {
        enum Item derives Schema  {
          case A, B
        }
        enum Item2 derives Schema {
          case A, B
        }
        val itemCodec: ProtobufCodec[Item] = Schema[Item2]
          .derive(deriver.modifier[Item2](oneOf(OneOfFlag.Inline)))
          .transform(
            _ match {
              case Item2.A => Item.A
              case Item2.B => Item.B
            },
            _ match {
              case Item.A => Item2.A
              case Item.B => Item2.B
            }
          )
        case class Container(items: List[Item]) derives Schema
        val containerCodec                 = Schema[Container].derive(deriver.instance(itemCodec))

        val original = Container(List(Item.A))
        val encoded  = containerCodec.encode(original)
        val decoded  = containerCodec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with Map field") {
        case class MapMessage(data: Map[String, Int]) derives Schema
        val codec = Schema[MapMessage].derive(deriver)

        val original = MapMessage(Map("key1" -> 1, "key2" -> 2, "key3" -> 3))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with Map field with a non-primitive key") {
        case class Key(value: String) derives Schema
        case class MapMessage(data: Map[Key, Int]) derives Schema
        val codec = Schema[MapMessage].derive(deriver)

        val original = MapMessage(Map(Key("key1") -> 1, Key("key2") -> 2, Key("key3") -> 3))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with empty objects in list") {
        case class EmptyItem(name: String, value: String) derives Schema
        case class EmptyContainer(items: List[EmptyItem]) derives Schema
        val emptyContainerCodec = Schema[EmptyContainer].derive(deriver)

        val original = EmptyContainer(List(EmptyItem("", "")))
        val encoded  = emptyContainerCodec.encode(original)
        val decoded  = emptyContainerCodec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("String Handling")(
      test("message with special characters") {
        case class TextMessage(text: String) derives Schema
        val codec = Schema[TextMessage].derive(deriver)

        val specialTexts = List(
          "Normal text",
          "Unicode测试",
          "Special!@#$%^&*()Characters",
          "Very Long String " * 100
        )

        val results = specialTexts.map { text =>
          val original = TextMessage(text)
          val encoded  = codec.encode(original)
          val decoded  = codec.decode(encoded)
          decoded
        }

        assert(results.map(_.text))(equalTo(specialTexts))
      },
      test("message with empty string fields") {
        case class EmptyStrings(name: String, description: String) derives Schema
        val codec = Schema[EmptyStrings].derive(deriver)

        val original = EmptyStrings("", "")
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("Opaque Types")(
      test("opaque type wrapper") {
        case class UserMessage(id: UserId, name: String) derives Schema
        val codec = Schema[UserMessage].derive(deriver)

        val original = UserMessage(UserId("user-123"), "John Doe")
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("Proteus Modifiers")(
      test("proteus oneof modifier forces enum to oneOf encoding") {
        enum RegularEnum derives Schema {
          case First
          case Second
          case Third
        }

        enum ForceOneOf derives Schema {
          case First
          case Second
          case Third
        }

        case class MessageWithEnum(id: Int, choice: RegularEnum) derives Schema
        case class MessageWithOneOf(id: Int, choice: ForceOneOf) derives Schema

        val enumCodec  = Schema[MessageWithEnum].derive(deriver)
        val oneOfCodec = Schema[MessageWithOneOf].derive(deriver.modifier[ForceOneOf](oneOf))

        val enumMessage  = MessageWithEnum(1, RegularEnum.First)
        val oneOfMessage = MessageWithOneOf(1, ForceOneOf.First)

        val enumEncoded  = enumCodec.encode(enumMessage)
        val oneOfEncoded = oneOfCodec.encode(oneOfMessage)

        val enumDecoded  = enumCodec.decode(enumEncoded)
        val oneOfDecoded = oneOfCodec.decode(oneOfEncoded)

        assert(enumDecoded)(equalTo(enumMessage)) &&
          assert(oneOfDecoded)(equalTo(oneOfMessage)) &&
          assert(enumEncoded)(not(equalTo(oneOfEncoded)))
      },
      test("proteus nested modifier") {
        case class NestedData(value: String, count: Int) derives Schema
        case class MessageWithNested(id: Int, data: NestedData) derives Schema

        val codec    = Schema[MessageWithNested].derive(deriver.modifier[NestedData](nested))
        val original = MessageWithNested(1, NestedData("test", 42))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("proteus oneof inline modifier") {
        enum InlineContact derives Schema {
          case Email(address: String)
          case Phone(number: String)
        }
        case class MessageWithInline(id: Int, contact: InlineContact) derives Schema

        val codec    = Schema[MessageWithInline].derive(deriver.modifier[InlineContact](oneOf(OneOfFlag.Inline)))
        val original = MessageWithInline(1, InlineContact.Email("test@example.com"))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("proteus excluded modifier skips field from codec") {
        case class MessageWithExcluded(id: Int, name: String, excluded: String) derives Schema
        case class MessageWithoutExcluded(id: Int, name: String) derives Schema

        val codecWithExcluded    =
          Schema[MessageWithExcluded].derive(deriver.modifier[MessageWithExcluded]("excluded", excluded))
        val codecWithoutExcluded = Schema[MessageWithoutExcluded].derive(deriver)

        val messageWithExcluded    = MessageWithExcluded(1, "test", "should be ignored")
        val messageWithoutExcluded = MessageWithoutExcluded(1, "test")

        val encodedWithExcluded    = codecWithExcluded.encode(messageWithExcluded)
        val encodedWithoutExcluded = codecWithoutExcluded.encode(messageWithoutExcluded)

        // The encoded messages should be the same size since excluded field is not serialized
        assert(encodedWithExcluded.length)(equalTo(encodedWithoutExcluded.length)) &&
          // And the encoded bytes should be identical
          assert(encodedWithExcluded)(equalTo(encodedWithoutExcluded))
      },
      test("proteus excluded modifier excludes enum cases from codec") {
        enum StatusWithExcluded derives Schema    {
          case Active
          case Inactive
          case Pending
        }
        enum StatusWithoutExcluded derives Schema {
          case Active
          case Pending
        }

        case class MessageWithExcludedEnum(status: StatusWithExcluded) derives Schema
        case class MessageWithoutExcludedEnum(status: StatusWithoutExcluded) derives Schema

        val codecWithExcluded    =
          Schema[MessageWithExcludedEnum].derive(deriver.modifier[StatusWithExcluded]("Inactive", excluded))
        val codecWithoutExcluded = Schema[MessageWithoutExcludedEnum].derive(deriver)

        val messageWithExcluded    = MessageWithExcludedEnum(StatusWithExcluded.Active)
        val messageWithoutExcluded = MessageWithoutExcludedEnum(StatusWithoutExcluded.Active)

        val encodedWithExcluded    = codecWithExcluded.encode(messageWithExcluded)
        val encodedWithoutExcluded = codecWithoutExcluded.encode(messageWithoutExcluded)

        // The encoded messages should be identical since excluded enum case is not present
        assert(encodedWithExcluded)(equalTo(encodedWithoutExcluded))
      },
      test("proteus excluded modifier excludes variant cases from codec") {
        enum ContactWithExcluded derives Schema    {
          case Email(address: String)
          case Phone(number: String)
          case Slack(workspace: String)
        }
        enum ContactWithoutExcluded derives Schema {
          case Email(address: String)
          case Slack(workspace: String)
        }

        case class MessageWithExcludedVariant(contact: ContactWithExcluded) derives Schema
        case class MessageWithoutExcludedVariant(contact: ContactWithoutExcluded) derives Schema

        val codecWithExcluded    =
          Schema[MessageWithExcludedVariant].derive(deriver.modifier[ContactWithExcluded]("Phone", excluded))
        val codecWithoutExcluded = Schema[MessageWithoutExcludedVariant].derive(deriver)

        val messageWithExcluded    = MessageWithExcludedVariant(ContactWithExcluded.Email("test@example.com"))
        val messageWithoutExcluded = MessageWithoutExcludedVariant(ContactWithoutExcluded.Email("test@example.com"))

        val encodedWithExcluded    = codecWithExcluded.encode(messageWithExcluded)
        val encodedWithoutExcluded = codecWithoutExcluded.encode(messageWithoutExcluded)

        // The encoded messages should be identical since excluded variant case is not present
        assert(encodedWithExcluded)(equalTo(encodedWithoutExcluded))
      },
      test("proteus excluded modifier with Map field allows decoding with default value") {
        case class MessageWithExcludedMap(id: Int, name: String, metadata: Map[String, String]) derives Schema

        val codec    = Schema[MessageWithExcludedMap].derive(deriver.modifier[MessageWithExcludedMap]("metadata", excluded))
        val original = MessageWithExcludedMap(123, "test", Map("key1" -> "value1", "key2" -> "value2"))

        val encoded = codec.encode(original)
        val decoded = codec.decode(encoded)

        assert(decoded.id)(equalTo(123)) &&
          assert(decoded.name)(equalTo("test")) &&
          assert(decoded.metadata)(equalTo(Map.empty[String, String]))
      }
    ),
    suite("Bytes Primitive")(
      test("message with bytes array encoding/decoding") {
        case class BytesMessage(id: Int, data: Array[Byte]) derives Schema
        val codec = Schema[BytesMessage].derive(deriver)

        val testData = Array[Byte](1, 2, 3, 4, 5, -1, -128, 127)
        val original = BytesMessage(1, testData)
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded.id)(equalTo(original.id)) &&
          assert(decoded.data)(equalTo(original.data))
      },
      test("message with empty byte array") {
        case class EmptyBytesMessage(id: Int, data: Array[Byte]) derives Schema
        val codec = Schema[EmptyBytesMessage].derive(deriver)

        val original = EmptyBytesMessage(1, Array.empty[Byte])
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded.id)(equalTo(original.id)) &&
          assert(decoded.data)(equalTo(original.data))
      },
      test("optional bytes None vs Some(empty) vs Some(data)") {
        case class OptionalBytesMessage(id: Int, data: Option[Array[Byte]]) derives Schema
        val codec = Schema[OptionalBytesMessage].derive(deriver)

        val messageWithNone  = OptionalBytesMessage(1, None)
        val messageWithEmpty = OptionalBytesMessage(1, Some(Array.empty[Byte]))
        val messageWithData  = OptionalBytesMessage(1, Some(Array[Byte](1, 2, 3)))

        val encodedNone  = codec.encode(messageWithNone)
        val encodedEmpty = codec.encode(messageWithEmpty)
        val encodedData  = codec.encode(messageWithData)

        val decodedNone  = codec.decode(encodedNone)
        val decodedEmpty = codec.decode(encodedEmpty)
        val decodedData  = codec.decode(encodedData)

        assert(decodedNone.id)(equalTo(messageWithNone.id)) &&
          assert(decodedNone.data)(equalTo(messageWithNone.data)) &&
          assert(decodedEmpty.id)(equalTo(messageWithEmpty.id)) &&
          assert(decodedEmpty.data.map(_.toSeq))(equalTo(messageWithEmpty.data.map(_.toSeq))) &&
          assert(decodedData.id)(equalTo(messageWithData.id)) &&
          assert(decodedData.data.map(_.toSeq))(equalTo(messageWithData.data.map(_.toSeq))) &&
          assert(encodedNone.length)(isLessThan(encodedEmpty.length)) &&
          assert(encodedEmpty.length)(isLessThan(encodedData.length))
      },
      test("message with bytes field in nested messages") {
        case class InnerMessage(data: Array[Byte], label: String) derives Schema
        case class OuterMessage(id: Int, inner: InnerMessage) derives Schema
        val codec = Schema[OuterMessage].derive(deriver)

        val testData = Array[Byte](-1, 0, 1, 127, -128)
        val original = OuterMessage(1, InnerMessage(testData, "test"))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded.id)(equalTo(original.id)) &&
          assert(decoded.inner.data)(equalTo(original.inner.data)) &&
          assert(decoded.inner.label)(equalTo(original.inner.label))
      },
      test("message with multiple bytes fields") {
        case class MultipleBytesMessage(id: Int, data1: Array[Byte], data2: Array[Byte]) derives Schema
        val codec = Schema[MultipleBytesMessage].derive(deriver)

        val data1    = Array[Byte](1, 2, 3)
        val data2    = Array[Byte](4, 5, 6, 7, 8)
        val original = MultipleBytesMessage(1, data1, data2)
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded.id)(equalTo(original.id)) &&
          assert(decoded.data1)(equalTo(original.data1)) &&
          assert(decoded.data2)(equalTo(original.data2))
      }
    ),
    suite("Error Handling")(
      test("decode invalid data returns error") {
        case class TestMessage(id: Int) derives Schema
        val codec = Schema[TestMessage].derive(deriver)

        val invalidBytes = Array[Byte](1, 2, 3, 4, 5)
        val decoded      = Try(codec.decode(invalidBytes)).toEither

        assert(decoded)(isLeft)
      },
      test("empty byte array decode returns defaults") {
        case class TestMessage(id: Int, name: String, active: Boolean) derives Schema
        val codec = Schema[TestMessage].derive(deriver)

        val emptyBytes = Array.empty[Byte]
        val decoded    = codec.decode(emptyBytes)

        assert(decoded)(equalTo(TestMessage(0, "", false)))
      }
    ),
    suite("Recursive Types")(
      test("message with recursive type") {
        case class Recursive(a: Int, b: Option[Recursive]) derives Schema
        val codec = Schema[Recursive].derive(deriver)

        val original = Recursive(1, Some(Recursive(2, None)))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("Transform")(
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

        val codec = Schema[MessageWithContact]
          .deriving(deriver)
          .instance(Schema[ContactWrapper].reflect.asRecord.get.typeName, transformedCodec)
          .derive

        val originalEmail = MessageWithContact(1, ContactWrapper(ContactType.Email("test@example.com")))
        val originalPhone = MessageWithContact(2, ContactWrapper(ContactType.Phone("555-1234")))

        val encodedEmail = codec.encode(originalEmail)
        val decodedEmail = codec.decode(encodedEmail)

        val encodedPhone = codec.encode(originalPhone)
        val decodedPhone = codec.decode(encodedPhone)

        assert(decodedEmail)(equalTo(originalEmail)) &&
          assert(decodedPhone)(equalTo(originalPhone))
      }
    ),
    suite("Derivation Flags")(
      test("AutoPrefixEnums flag does not affect codec behavior") {
        enum Status derives Schema { case Active, Inactive, Pending }
        case class StandardStatusMessage(status: Status) derives Schema
        case class AutoPrefixStatusMessage(status: Status) derives Schema

        val standardCodec   = Schema[StandardStatusMessage].derive(deriver)
        val autoPrefixCodec = Schema[AutoPrefixStatusMessage].derive(deriverWithAutoPrefixEnums)

        val testCases = List(Status.Active, Status.Inactive, Status.Pending)

        val results = testCases.map { status =>
          val standardMessage   = StandardStatusMessage(status)
          val autoPrefixMessage = AutoPrefixStatusMessage(status)

          val standardEncoded   = standardCodec.encode(standardMessage)
          val autoPrefixEncoded = autoPrefixCodec.encode(autoPrefixMessage)

          val standardDecoded   = standardCodec.decode(standardEncoded)
          val autoPrefixDecoded = autoPrefixCodec.decode(autoPrefixEncoded)

          // The enum values should encode/decode identically regardless of prefix flag
          (standardDecoded.status, autoPrefixDecoded.status, standardEncoded sameElements autoPrefixEncoded)
        }

        assert(results.map(_._1))(equalTo(testCases)) &&
          assert(results.map(_._2))(equalTo(testCases)) &&
          assert(results.forall(_._3))(isTrue)
      }
    )
  )
}
