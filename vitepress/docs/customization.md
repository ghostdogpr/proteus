# Customization

Customization is sometimes needed when the generated Protobuf schema does not match what you want.
There are several ways to customize the generated schema:
- [Codec transformations](/customization#codec-transformations) allow using a different Scala type to generate the Protobuf type, as long as you provide functions to transform from and to the original type.
- [Modifiers](/customization#modifiers) allow changing the generated schema for common use cases such as renames, comments, reserved indexes, etc.
- [Derivation flags](/customization#derivation-flags) allow configuring the derivation process.

## Codec transformations

Let's imagine you have the following case class:
```scala
case class Order(items: Map[String, Int])
```
It will generate the following Protobuf schema:
```proto
message Order {
    map<string, int32> items = 1;
}
```
But you want to generate a different Protobuf schema, for example:
```proto
message Order {
    repeated Item items = 1;
}

message Item {
    string id = 1;
    int32 count = 2;
}
```
You could change your case class to match this, but you might also want to keep the original type for other purposes.
In that case, you can create separate Scala types that match the expected Protobuf schema:
```scala
object Proto {
  case class Item(id: String, count: Int)
  case class Order(items: List[Item])
}
```
Then you can create a codec for the original type by calling `transform` on the codec for the Protobuf type and providing functions to transform from and to the original type.
```scala
val codec: ProtobufCodec[Order] =
  ProtobufCodec
    .derived[Proto.Order](using ProtobufDeriver)
    .transform[Order](
      proto => Order(proto.items.map(i => i.id -> i.count).toMap),
      order => Proto.Order(order.items.map((id, count) => Proto.Item(id, count)).toList)
    )
```
Let's check the rendered schema:
```scala
println(codec.render())
// syntax = "proto3";
// 
// message Order {
//     repeated Item items = 1;
// }
// 
// message Item {
//     string id = 1;
//     int32 count = 2;
// }
```

But what if your type is used by other types? How do you tell Proteus to use the correct codec? This is done by calling `.instance` on the deriver and providing the codec you want to use. You need an instance of `Schema` for the type to apply the instance override to it.

```scala
val deriver = ProtobufDeriver.instance(codec)
```
Now everywhere you use `deriver` (rather than the default `ProtobufDeriver`), your custom codec will be used in place of the default one.

## Modifiers
Creating a custom codec is useful, but it is boilerplate, and we want to avoid it when possible.
For that reason, Proteus provides **modifiers** that allow you to easily change the generated schema for common use cases without having to touch the Scala types.

Let's see a simple example. We have this case class:
```scala
case class Person(name: String, age: Int)
```
But we want it to be named "User" in the Protobuf schema. There is no need to create a case class `User` for such a simple use case. Instead, you can apply a modifier to the deriver like this:
```scala
import proteus.Modifiers.*

val deriver = ProtobufDeriver.modifier[Person](rename("User"))

println(ProtobufCodec.derived[Person](using deriver).render())
// syntax = "proto3";
// 
// message User {
//     string name = 1;
//     int32 age = 2;
// }
```
If you wanted to rename the `name` field instead, you could do:
```scala
import proteus.Modifiers.*

val deriver = ProtobufDeriver.modifier[Person]("name", rename("full_name"))

println(ProtobufCodec.derived[Person](using deriver).render())
// syntax = "proto3";
// 
// message Person {
//     string full_name = 1;
//     int32 age = 2;
// }
```
Here are the different types of modifiers you can apply:
- `excluded`: Excludes a field or an enum member from the protobuf type.
- `nested`: Nests a type inside its parent message instead of creating it at the root level.
- `unnested`: Forces a type to be created at the root level and prevent it from being nested by the `OneOfFlag.Nested` flag (see below).
- `oneOf`: Forces a type to be encoded as a `oneof` rather than an enum.
- `oneOf(flags: OneOfFlag*)`: Controls how `oneof` types are encoded—see below for more details.
- `enumPrefix(prefix: String)`: Prefixes enum members with a string.
- `enumSuffix(suffix: String)`: Suffixes enum members with a string.
- `comment(comment: String)`: Adds a comment to a type, field, or enum member.
- `rename(name: String)`: Renames a type, field, or enum member.
- `reserved(indexes: Int*)`: Adds some reserved indexes to a type. Those reserved indexes will be skipped when deriving the protobuf type. If this modifier is applied to a field, the field will use the given index(es).

There are two possible `OneOfFlag` values:
- `Inline`: Inlines the `oneof` as a single field inside the parent message.
```proto
message Example {
  int32 field = 1;
  oneof address { // inline oneof field
    Email email = 2;
    Phone phone = 3;
  }
}
```
- `Nested`: Makes the `oneof` a separate type nested inside the parent message.
```proto
message Example {
  message Address { // nested oneof type
    oneof value {
      Email email = 1;
      Phone phone = 2;
    }
  }
  int32 field = 1;
  Address address = 2;
}
```

## Derivation flags

Sometimes you might want some specific behavior to apply to all types during derivation, without having to create a custom codec or modifier for each type.
For that, Proteus provides **derivation flags** that allow configuring the derivation process.
```scala
import proteus.ProtobufDeriver.DerivationFlag

val deriver = ProtobufDeriver.enable(DerivationFlag.AutoPrefixEnums)
```

Here are the different types of derivation flags you can apply:
- `OptionalAsOneOf`: Instead of using the `optional` keyword, optional fields will be encoded as a `oneof` field with two cases: one of type `Empty` (an empty message) and one for the actual value.
- `AutoPrefixEnums`: Automatically prefix the enum members with the type name.
- `AutoSuffixEnums`: Automatically suffix the enum members with the type name.
- `NestedOneOf`: All types used in `oneof` fields will be encoded as nested types inside the parent message. This is equivalent to applying the `oneOf(Nested)` modifier to all types used in `oneof` fields.

Let's check a quick example.
```scala
enum Status { case Active, Inactive, Pending }

val deriver = ProtobufDeriver.enable(DerivationFlag.AutoPrefixEnums)

println(ProtobufCodec.derived[Status](using deriver).render())
// syntax = "proto3";
// 
// enum Status {
//     STATUS_ACTIVE = 0;
//     STATUS_INACTIVE = 1;
//     STATUS_PENDING = 2;
// }
```
As you can see, the enum members are prefixed with "STATUS_".
