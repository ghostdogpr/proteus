package proteus
package json

import io.circe.*
import io.circe.syntax.*
import zio.blocks.schema.Schema
import zio.test.*

import proteus.*
import proteus.Modifiers.*

object JsonSpec extends ZIOSpecDefault {
  given ProtobufDeriver = ProtobufDeriver
  given Registry        = Registry.empty

  def spec = suite("JsonSpec")(
    suite("Primitive Types")(
      test("toJson serializes Int") {
        case class IntMessage(value: Int) derives Schema, ProtobufCodec
        val instance = IntMessage(42)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"value":42}""")
      },
      test("toJson serializes Long") {
        case class LongMessage(value: Long) derives Schema, ProtobufCodec
        val instance = LongMessage(123456789L)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"value":123456789}""")
      },
      test("toJson serializes String") {
        case class StringMessage(value: String) derives Schema, ProtobufCodec
        val instance = StringMessage("hello world")
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"value":"hello world"}""")
      },
      test("toJson serializes Boolean true") {
        case class BoolMessage(value: Boolean) derives Schema, ProtobufCodec
        val instance = BoolMessage(true)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"value":true}""")
      },
      test("toJson serializes Boolean false") {
        case class BoolMessage(value: Boolean) derives Schema, ProtobufCodec
        val instance = BoolMessage(false)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"value":false}""")
      },
      test("toJson serializes Double") {
        case class DoubleMessage(value: Double) derives Schema, ProtobufCodec
        val instance = DoubleMessage(3.14159)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"value":3.14159}""")
      },
      test("toJson serializes Float") {
        case class FloatMessage(value: Float) derives Schema, ProtobufCodec
        val instance = FloatMessage(2.5f)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"value":2.5}""")
      },
      test("toJson handles String with special characters") {
        case class StringMessage(value: String) derives Schema, ProtobufCodec
        val instance = StringMessage("hello\nworld\"test")
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"value":"hello\nworld\"test"}""")
      }
    ),
    suite("Enum Types")(
      test("toJson serializes enum") {
        enum Status derives Schema, ProtobufCodec { case Active, Inactive, Pending }
        case class StatusMessage(status: Status) derives Schema, ProtobufCodec
        val instance = StatusMessage(Status.Active)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"status":"ACTIVE"}""")
      },
      test("toJson serializes enum with different values") {
        enum Priority derives Schema, ProtobufCodec { case Low, Medium, High }
        case class PriorityMessage(priority: Priority) derives Schema, ProtobufCodec

        val low    = PriorityMessage(Priority.Low)
        val medium = PriorityMessage(Priority.Medium)
        val high   = PriorityMessage(Priority.High)

        val lowJson    = low.asJson.noSpaces
        val mediumJson = medium.asJson.noSpaces
        val highJson   = high.asJson.noSpaces

        assertTrue(lowJson == """{"priority":"LOW"}""") &&
          assertTrue(mediumJson == """{"priority":"MEDIUM"}""") &&
          assertTrue(highJson == """{"priority":"HIGH"}""")
      },
      test("toJson serializes enum with prefix modifier") {
        enum Status derives Schema, ProtobufCodec { case Active, Inactive }
        case class StatusMessage(status: Status) derives Schema, ProtobufCodec
        given ProtobufDeriver = ProtobufDeriver.modifier[Status](enumPrefix("USER"))
        val instance          = StatusMessage(Status.Active)
        val result            = instance.asJson.noSpaces
        assertTrue(result == """{"status":"USER_ACTIVE"}""")
      }
    ),
    suite("Optional Types")(
      test("toJson serializes Some value") {
        case class OptionalMessage(value: Option[String]) derives Schema, ProtobufCodec
        val instance = OptionalMessage(Some("test"))
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"value":"test"}""")
      },
      test("toJson serializes None as null") {
        case class OptionalMessage(value: Option[String]) derives Schema, ProtobufCodec
        val instance = OptionalMessage(None)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{}""")
      },
      test("toJson serializes optional Int") {
        case class OptionalIntMessage(value: Option[Int]) derives Schema, ProtobufCodec
        val some = OptionalIntMessage(Some(42))
        val none = OptionalIntMessage(None)

        val someJson = some.asJson.noSpaces
        val noneJson = none.asJson.noSpaces

        assertTrue(someJson == """{"value":42}""") &&
          assertTrue(noneJson == """{}""")
      }
    ),
    suite("Message Types")(
      test("toJson serializes simple message") {
        case class SimpleMessage(id: Int, name: String, active: Boolean) derives Schema, ProtobufCodec
        val instance = SimpleMessage(1, "test", true)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"id":1,"name":"test","active":true}""")
      },
      test("toJson serializes nested message") {
        case class Address(street: String, city: String) derives Schema, ProtobufCodec
        case class Person(name: String, address: Address) derives Schema, ProtobufCodec
        val instance = Person("John", Address("Main St", "NYC"))
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"name":"John","address":{"street":"Main St","city":"NYC"}}""")
      },
      test("toJson serializes message with optional nested message") {
        case class Contact(email: String, phone: Option[String]) derives Schema, ProtobufCodec
        case class User(name: String, contact: Option[Contact]) derives Schema, ProtobufCodec

        val withContact    = User("Alice", Some(Contact("alice@test.com", Some("123-456"))))
        val withoutContact = User("Bob", None)

        val withJson    = withContact.asJson.noSpaces
        val withoutJson = withoutContact.asJson.noSpaces

        assertTrue(withJson == """{"name":"Alice","contact":{"email":"alice@test.com","phone":"123-456"}}""") &&
          assertTrue(withoutJson == """{"name":"Bob"}""")
      }
    ),
    suite("Collection Types")(
      test("toJson serializes List[Int]") {
        case class ListMessage(values: List[Int]) derives Schema, ProtobufCodec
        val instance = ListMessage(List(1, 2, 3))
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"values":[1,2,3]}""")
      },
      test("toJson serializes empty List") {
        case class ListMessage(values: List[String]) derives Schema, ProtobufCodec
        val instance = ListMessage(List.empty)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"values":[]}""")
      },
      test("toJson serializes List of messages") {
        case class Item(id: Int, name: String) derives Schema, ProtobufCodec
        case class ItemList(items: List[Item]) derives Schema, ProtobufCodec
        val instance = ItemList(List(Item(1, "first"), Item(2, "second")))
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"items":[{"id":1,"name":"first"},{"id":2,"name":"second"}]}""")
      },
      test("toJson serializes Vector[String]") {
        case class VectorMessage(values: Vector[String]) derives Schema, ProtobufCodec
        val instance = VectorMessage(Vector("a", "b", "c"))
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"values":["a","b","c"]}""")
      }
    ),
    suite("Map Types")(
      test("toJson serializes Map[String, Int]") {
        case class MapMessage(data: Map[String, Int]) derives Schema, ProtobufCodec
        val instance = MapMessage(Map("one" -> 1, "two" -> 2))
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"data":[{"one":1},{"two":2}]}""" || result == """{"data":[{"two":2},{"one":1}]}""")
      },
      test("toJson serializes Map[Int, Int]") {
        case class MapMessage(data: Map[Int, Int]) derives Schema, ProtobufCodec
        val instance = MapMessage(Map(1 -> 1, 2 -> 2))
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"data":[{"1":1},{"2":2}]}""" || result == """{"data":[{"2":2},{"1":1}]}""")
      },
      test("toJson serializes empty Map") {
        case class MapMessage(data: Map[String, String]) derives Schema, ProtobufCodec
        val instance = MapMessage(Map.empty)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"data":[]}""")
      },
      test("toJson serializes Map with complex values") {
        case class Value(amount: Int, currency: String) derives Schema, ProtobufCodec
        case class MapMessage(data: Map[String, Value]) derives Schema, ProtobufCodec
        val instance = MapMessage(Map("price" -> Value(100, "USD")))
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"data":[{"price":{"amount":100,"currency":"USD"}}]}""")
      }
    ),
    suite("Bytes and Binary Data")(
      test("toJson serializes byte array") {
        case class BytesMessage(data: Array[Byte]) derives Schema, ProtobufCodec
        val instance = BytesMessage(Array(1, 2, 3, 4, 5))
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"data":"<bytes>"}""")
      },
      test("toJson serializes empty byte array") {
        case class BytesMessage(data: Array[Byte]) derives Schema, ProtobufCodec
        val instance = BytesMessage(Array.empty)
        val result   = instance.asJson.noSpaces
        assertTrue(result == """{"data":"<bytes>"}""")
      }
    ),
    suite("OneOf and Variant Types")(
      test("toJson serializes oneOf inline variant") {
        enum ContactType derives Schema, ProtobufCodec {
          case Email(address: String)
          case Phone(number: String)
        }
        case class Contact(info: ContactType) derives Schema, ProtobufCodec
        given ProtobufDeriver = ProtobufDeriver.modifier[ContactType](oneOf(OneOfFlag.Inline))

        val email = Contact(ContactType.Email("test@example.com"))
        val phone = Contact(ContactType.Phone("123-456-7890"))

        val emailJson = email.asJson.noSpaces
        val phoneJson = phone.asJson.noSpaces

        assertTrue(emailJson == """{"email":{"address":"test@example.com"}}""") &&
          assertTrue(phoneJson == """{"phone":{"number":"123-456-7890"}}""")
      },
      test("toJson serializes nested oneOf") {
        enum Status derives Schema, ProtobufCodec {
          case Active(since: String)
          case Inactive(reason: String)
        }
        case class User(name: String, status: Status) derives Schema, ProtobufCodec
        given ProtobufDeriver = ProtobufDeriver.modifier[Status](oneOf(OneOfFlag.Nested))

        val user   = User("Alice", Status.Active("2023-01-01"))
        val result = user.asJson.noSpaces

        assertTrue(result == """{"name":"Alice","status":{"active":{"since":"2023-01-01"}}}""")
      }
    ),
    suite("Transform and Recursive Types")(
      test("toJson serializes simple wrapper types") {
        case class UserId(value: Int) derives Schema, ProtobufCodec
        case class User(id: UserId, name: String) derives Schema, ProtobufCodec

        val instance = User(UserId(123), "Alice")
        val result   = instance.asJson.noSpaces

        assertTrue(result == """{"id":{"value":123},"name":"Alice"}""")
      },
      test("toJson serializes recursive types") {
        case class TreeNode(value: Int, children: List[TreeNode]) derives Schema, ProtobufCodec
        val instance = TreeNode(1, List(TreeNode(2, Nil), TreeNode(3, Nil)))
        val result   = instance.asJson.noSpaces

        assertTrue(result == """{"value":1,"children":[{"value":2,"children":[]},{"value":3,"children":[]}]}""")
      }
    ),
    suite("Field Renaming and Modifiers")(
      test("toJson respects field rename modifier") {
        case class User(id: Int, name: String) derives Schema, ProtobufCodec
        given ProtobufDeriver =
          ProtobufDeriver
            .modifier[User]("id", rename("user_id"))
            .modifier[User]("name", rename("full_name"))

        val instance = User(123, "Alice")
        val result   = instance.asJson.noSpaces

        assertTrue(result == """{"userId":123,"fullName":"Alice"}""")
      },
      test("toJson respects excluded modifier") {
        case class User(id: Int, name: String, secret: String) derives Schema, ProtobufCodec
        given ProtobufDeriver = ProtobufDeriver.modifier[User]("secret", excluded)
        val instance          = User(123, "Alice", "top-secret")
        val result            = instance.asJson.noSpaces

        assertTrue(result == """{"id":123,"name":"Alice"}""")
      }
    ),
    suite("Edge Cases")(
      test("toJson handles zero values") {
        case class Zeros(intVal: Int, stringVal: String, boolVal: Boolean) derives Schema, ProtobufCodec
        val instance = Zeros(0, "", false)
        val result   = instance.asJson.noSpaces

        assertTrue(result == """{"intVal":0,"stringVal":"","boolVal":false}""")
      },
      test("toJson omits null values") {
        case class B(c: Int) derives Schema, ProtobufCodec
        case class A(a: Int, b: B) derives Schema, ProtobufCodec
        val instance = A(1, null)
        val result   = instance.asJson.noSpaces

        assertTrue(result == """{"a":1}""")
      },
      test("toJson handles complex nested structure") {
        case class Address(street: String, city: String, zip: Option[String]) derives Schema, ProtobufCodec
        case class Contact(email: String, phones: List[String]) derives Schema, ProtobufCodec
        case class Person(
          id: Int,
          name: String,
          address: Address,
          contact: Option[Contact],
          tags: Map[String, String],
          active: Boolean
        ) derives Schema,
            ProtobufCodec

        val instance = Person(
          1,
          "John Doe",
          Address("123 Main St", "NYC", Some("10001")),
          Some(Contact("john@example.com", List("555-1234", "555-5678"))),
          Map("role" -> "admin", "dept" -> "engineering"),
          true
        )

        val result = instance.asJson.noSpaces

        assertTrue(
          result == """{"id":1,"name":"John Doe","address":{"street":"123 Main St","city":"NYC","zip":"10001"},"contact":{"email":"john@example.com","phones":["555-1234","555-5678"]},"tags":[{"role":"admin"},{"dept":"engineering"}],"active":true}""" ||
            result == """{"id":1,"name":"John Doe","address":{"street":"123 Main St","city":"NYC","zip":"10001"},"contact":{"email":"john@example.com","phones":["555-1234","555-5678"]},"tags":[{"dept":"engineering"},{"role":"admin"}],"active":true}"""
        )
      }
    ),
    suite("Registry")(
      test("custom encoder in registry overrides default encoding") {
        case class CustomMessage(value: Int) derives Schema, ProtobufCodec

        val customEncoder: Encoder[CustomMessage] = (msg: CustomMessage) => Json.obj("custom" -> Json.fromInt(msg.value * 100))

        given Registry = Registry.empty.add(customEncoder)

        val instance = CustomMessage(42)
        val result   = instance.asJson.noSpaces

        assertTrue(result == """{"custom":4200}""")
      },
      test("custom encoder for primitive type in message") {
        case class Message(id: Int, name: String) derives Schema, ProtobufCodec

        val customEncoder: Encoder[Message] = (msg: Message) =>
          Json.obj(
            "identifier" -> Json.fromInt(msg.id),
            "label"      -> Json.fromString(msg.name.toUpperCase)
          )

        given Registry = Registry.empty.add(customEncoder)

        val instance = Message(123, "test")
        val result   = instance.asJson.noSpaces

        assertTrue(result == """{"identifier":123,"label":"TEST"}""")
      },
      test("custom encoder for enum changes representation") {
        enum Status derives Schema, ProtobufCodec { case Active, Inactive, Pending }
        case class StatusMessage(status: Status) derives Schema, ProtobufCodec

        val customEncoder: Encoder[Status] = (status: Status) => Json.fromInt(status.ordinal + 1000)

        given Registry = Registry.empty.add(customEncoder)

        val instance = StatusMessage(Status.Active)
        val result   = instance.asJson.noSpaces

        assertTrue(result == """{"status":1000}""")
      }
    )
  )
}
