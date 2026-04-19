# Getting started

**Proteus** is a **[Scala](https://www.scala-lang.org/) open source library** for working with **[Protobuf](https://protobuf.dev/)** and **[gRPC](https://grpc.io/)**.

It is designed to be **code-first**, meaning that it is able to generate Protobuf codecs and .proto files directly from your Scala code without code generation.

It also provides a **declarative way to define gRPC services** in Scala, a bit like [tapir](https://tapir.softwaremill.com/en/latest/) does for HTTP services. You can define messages, RPCs, and services in Scala, then generate clients and servers for them, using a variety of backends (direct style, Future, ZIO, fs2, Ox).

It is available for Scala 3.3.x LTS and later versions. The core module is available for both Scala JVM and Scala.js.

::: warning Why not use code generation?
Let's address the elephant in the room: why not use code generation like everyone else? Check the [FAQ](/faq#why-not-use-code-generation) for a detailed answer.
:::

## Our first Protobuf codec

We first need to add the Proteus library to our project. If you are using sbt, you can add the following dependency to your `build.sbt` file:

```scala
"com.github.ghostdogpr" %% "proteus-core" % "@VERSION@"
```
Let's create a simple case class we would like to encode and decode into Protobuf.

```scala
case class Person(name: String, age: Int)
```

Now, we can derive a Protobuf codec for it.

```scala
import proteus.*

given ProtobufDeriver = ProtobufDeriver

val codec = ProtobufCodec.derived[Person]
```

Let's use this codec to encode and decode a `Person` instance.

```scala
val person = Person("John Doe", 30) // : Person = Person(John Doe,30)
val encoded = codec.encode(person)  // : Array[Byte] = Array(10, 8, 74, 111, 104, 110, 32, 68, 111, 101, 16, 30)
val decoded = codec.decode(encoded) // : Person = Person(John Doe,30)

assert(decoded == person)
```

We can also take a look at the associated Protobuf schema.

```scala
println(codec.render())
// syntax = "proto3";
// 
// message Person {
//     string name = 1;
//     int32 age = 2;
// }
// 
```

## How does it work?

Let's rewind and explain in detail what we just did.

First we created a `given` instance of `ProtobufDeriver`. This is an object that defines how the derivation should work.
You can use it to configure derivation flags that will affect the produced schema, but also register custom instances and modifiers for specific types.
Here we are just using `ProtobufDeriver`, which is the default instance.

Then, we called `ProtobufCodec.derived[Person]` to create a `ProtobufCodec` for the `Person` case class. It requires a `ProtobufDeriver` instance in scope, which is why we created one first.

You can also use the `derives` keyword to create a `ProtobufCodec` for `Person` that will always be in scope (again, you need a `given ProtobufDeriver` instance in scope).
```scala
case class Person(name: String, age: Int) derives ProtobufCodec
```
If we do so, we can use `ProtobufCodec[Person]` without `.derived` since we already have a `ProtobufCodec` instance for `Person`.

```scala
val codec = ProtobufCodec[Person]
```

That codec can now be used to `encode`, `decode`, and `render` the `Person` case class!

::: tip Supported types
Proteus derivation supports the following types:
- Primitive types: `String`, `Int`, `Long`, `Float`, `Double`, `Boolean`
- Collections: `List`, `Vector`, `Set`, `Map` (anything supported by zio-blocks, see below)
- Others: `Option`, `Array[Byte]` (mapped to `bytes` in Protobuf)
- Case classes, sealed traits, enums
- Recursive types
- Opaque types
:::

## Under the hood

Proteus uses a typeclass called `Schema` from the [zio-blocks](https://github.com/zio/zio-blocks) library to gather information about Scala types.
Note that this library is very lightweight and has no dependencies (it does not require zio).

`ProtobufCodec.derived` requires a `Schema` instance to function. If one is available in scope, it will use it; otherwise, it will derive one automatically.

To improve compile-time efficiency, you should avoid deriving a `Schema` instance multiple times for the same type. By adding `derives Schema` to your case class, enum or sealed trait, a `Schema` instance will be created in the companion object and always be available in scope.

```scala
import zio.blocks.schema.*

case class Person(name: String, age: Int) derives Schema
```

## Where to go next?

There will be times when we want the generated Protobuf schema to be different from the default one, whether it's for convenience or backward compatibility. Various customization options are available to achieve this and are detailed in the [Customization](/customization) section.

The [gRPC services](/grpc-services) section details how to define complete gRPC services in Scala and use them to create clients and servers using the backend of your choice.

Even if we don't use .proto files to generate our code, it is still useful to have them, whether it's for documenting our API or checking what changes were made to the schema. The [Proto file generation](/proto-file-generation) section details how to generate .proto files from your codecs.

The [proteus-diff](/proteus-diff) CLI detects breaking changes between `.proto` versions.

Finally, take a look at the [examples](https://github.com/ghostdogpr/proteus/tree/main/examples/src/main/scala/proteus/examples) in the GitHub repository for more detailed examples.
