# Getting Started

**Proteus** is a **[Scala](https://www.scala-lang.org/) open source library** for working with **[Protobuf](https://protobuf.dev/)** and **[gRPC](https://grpc.io/)**.

It is designed to be **code-first**, meaning that it is able to generate Protobuf codecs and .proto files directly from your Scala code.

It also provide a **declarative way to define gRPC services** in Scala, a bit like [tapir](https://tapir.softwaremill.com/en/latest/) does for HTTP services. You can define messages, RPCs and services in Scala, then generate clients and servers for it, using a variety of backends (direct style, Future, ZIO, fs2).

::: warning Why not using code generation?
Let's address the elephant in the room: why not using code generation like everyone else? Check the [FAQ](/faq#why-not-using-code-generation) for a detailed answer.
:::

## Our first Protobuf codec

We first need to add the Proteus library to our project. If you are using sbt, you can add the following dependency to your `build.sbt` file:

```scala
libraryDependencies += "com.github.ghostdogpr" %% "proteus" % "0.1.0"
```
Let's creata a simple case class we would like to encode and decode into Protobuf.

```scala
case class Person(name: String, age: Int)
```

Now, we can derive a Protobuf codec for it.

```scala
import proteus.*
import zio.blocks.schema.*

val codec = Schema.derived[Person].derive(ProtobufDeriver)
```

Let's use this codec to encode and decode a `Person` instance.

```scala
val person = Person("John Doe", 30)
val encoded = codec.encode(person)
val decoded = codec.decode(encoded)

assert(decoded == person)
```