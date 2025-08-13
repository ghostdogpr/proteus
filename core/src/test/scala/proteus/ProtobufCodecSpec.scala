package proteus

import java.time.*

import scala.util.Try

import zio.blocks.schema.{Schema, TypeName}
import zio.test.*
import zio.test.Assertion.*

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

  val testDateTimeSchema: ProtobufCodec[OffsetDateTime] =
    Schema[DateTimeWrapper]
      .derive(ProtobufDeriver)
      .transform[OffsetDateTime](
        wrapper => {
          val millis = wrapper.currentTimeMillis
          if (millis == 0) DateTime.min else DateTime.ofEpochMilli(millis)
        },
        dt => DateTimeWrapper(dt.toEpochMilli)
      )

  def spec = suite("ProtobufCodecSpec")(
    suite("Simple Types")(
      test("basic message with primitives") {
        case class SimpleMessage(id: Int, name: String, active: Boolean) derives Schema
        val codec = Schema[SimpleMessage].derive(ProtobufDeriver)

        val original = SimpleMessage(42, "test", true)
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with default values") {
        case class DefaultMessage(id: Int, name: String, active: Boolean) derives Schema
        val codec = Schema[DefaultMessage].derive(ProtobufDeriver)

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
        val codec = Schema[StatusMessage].derive(ProtobufDeriver)

        val variants = List(Status.Active, Status.Inactive, Status.Pending)
        val results  = variants.map { status =>
          val original = StatusMessage(1, status)
          val encoded  = codec.encode(original)
          val decoded  = codec.decode(encoded)
          decoded
        }

        assert(results.map(_.status))(equalTo(variants))
      },
      test("oneOf field variants") {
        enum ContactInfo derives Schema {
          case Email(address: String)
          case Phone(number: String)
          case Social(platform: String, handle: String)
        }
        case class ContactMessage(contact: ContactInfo) derives Schema
        val codec = Schema[ContactMessage].derive(ProtobufDeriver)

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

        val codec = Schema[TimeMessage].deriving(ProtobufDeriver).instance(TypeName.offsetDateTime, testDateTimeSchema).derive

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
        val codec = Schema[OptionalMessage].derive(ProtobufDeriver)

        val withSome = OptionalMessage(1, Some("present"))
        val withNone = OptionalMessage(2, None)

        val encodedSome = codec.encode(withSome)
        val decodedSome = codec.decode(encodedSome)

        val encodedNone = codec.encode(withNone)
        val decodedNone = codec.decode(encodedNone)

        assert(decodedSome)(equalTo(withSome)) &&
          assert(decodedNone)(equalTo(withNone))
      }
    ),
    suite("Nested Messages")(
      test("message with nested message") {
        case class Inner(value: String) derives Schema
        case class Outer(id: Int, inner: Inner) derives Schema
        val outerCodec = Schema[Outer].derive(ProtobufDeriver)

        val original = Outer(1, Inner("nested"))
        val encoded  = outerCodec.encode(original)
        val decoded  = outerCodec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("Collections")(
      test("message with List[Int]") {
        case class NumberMessage(name: String, numbers: List[Int]) derives Schema
        val codec = Schema[NumberMessage].derive(ProtobufDeriver)

        val original = NumberMessage("test", List(1, 2, 3, 4, 5))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with empty List[Int]") {
        case class EmptyListMessage(name: String, numbers: List[Int]) derives Schema
        val codec = Schema[EmptyListMessage].derive(ProtobufDeriver)

        val original = EmptyListMessage("empty", List.empty)
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with List[Int] containing zeros") {
        case class ZeroListMessage(name: String, numbers: List[Int]) derives Schema
        val codec = Schema[ZeroListMessage].derive(ProtobufDeriver)

        val original = ZeroListMessage("zeros", List(0, 1, 0, 2, 0))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with List of messages") {
        case class Item(name: String, value: Int) derives Schema
        case class Container(items: List[Item]) derives Schema
        val containerCodec = Schema[Container].derive(ProtobufDeriver)

        val original = Container(List(Item("a", 1), Item("b", 2), Item("c", 3)))
        val encoded  = containerCodec.encode(original)
        val decoded  = containerCodec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with Map field") {
        case class MapMessage(data: Map[String, Int]) derives Schema
        val codec = Schema[MapMessage].derive(ProtobufDeriver)

        val original = MapMessage(Map("key1" -> 1, "key2" -> 2, "key3" -> 3))
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      },
      test("message with empty objects in list") {
        case class EmptyItem(name: String, value: String) derives Schema
        case class EmptyContainer(items: List[EmptyItem]) derives Schema
        val emptyContainerCodec = Schema[EmptyContainer].derive(ProtobufDeriver)

        val original = EmptyContainer(List(EmptyItem("", "")))
        val encoded  = emptyContainerCodec.encode(original)
        val decoded  = emptyContainerCodec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("String Handling")(
      test("message with special characters") {
        case class TextMessage(text: String) derives Schema
        val codec = Schema[TextMessage].derive(ProtobufDeriver)

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
        val codec = Schema[EmptyStrings].derive(ProtobufDeriver)

        val original = EmptyStrings("", "")
        val encoded  = codec.encode(original)
        val decoded  = codec.decode(encoded)

        assert(decoded)(equalTo(original))
      }
    ),
    suite("Error Handling")(
      test("decode invalid data returns error") {
        case class TestMessage(id: Int) derives Schema
        val codec = Schema[TestMessage].derive(ProtobufDeriver)

        val invalidBytes = Array[Byte](1, 2, 3, 4, 5)
        val decoded      = Try(codec.decode(invalidBytes)).toEither

        assert(decoded)(isLeft)
      },
      test("empty byte array decode returns defaults") {
        case class TestMessage(id: Int, name: String, active: Boolean) derives Schema
        val codec = Schema[TestMessage].derive(ProtobufDeriver)

        val emptyBytes = Array.empty[Byte]
        val decoded    = codec.decode(emptyBytes)

        assert(decoded)(equalTo(TestMessage(0, "", false)))
      }
    )
  )
}
