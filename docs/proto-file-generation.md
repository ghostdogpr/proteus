# Proto file generation

Even though Proteus is designed to be code-first, it is useful to generate `.proto` files, whether it's for sharing the API schema with consumers, ensuring backward compatibility, or simply to keep track of how the schema evolves over time.

## Dependencies

We saw in [Getting started](/getting-started) how to render a single type, but typically we will want to render multiple types together.
For that, we can use the `Dependency` case class.
It is a collection of types that are related to each other and that we want to render together.
It can be used to bundle types together and write them to a single .proto file.
```scala
import proteus.*

given ProtobufDeriver = ProtobufDeriver // your deriver instance

case class Address(street: String, city: String) derives ProtobufCodec
case class Person(name: String, address: Address) derives ProtobufCodec

val entities = Dependency("entities", "com.example").add[Person]
println(entities.render(Nil))
// syntax = "proto3";
// 
// package com.example;
// 
// message Person {
//     string name = 1;
//     Address address = 2;
// }
// 
// message Address {
//     string street = 1;
//     string city = 2;
// }
// 
```
You don't need to add all the types manually to the dependency: adding a type will automatically add all the types it depends on.
There is also `renderToFile` to write the dependency directly to a file (in this example, `entities.proto`).

Dependencies can also depend on other dependencies.
```scala
val common = Dependency("common", "com.example").add[Address]

val entitiesWithDependency = entities.dependsOn(common)
println(entitiesWithDependency.render(Nil))
// syntax = "proto3";
// 
// package com.example;
// 
// import "common.proto";
// 
// message Person {
//     string name = 1;
//     Address address = 2;
// }
// 
```
As you can see, `Address` is no longer there and is replaced by the import of the `common` dependency.

::: warning
Proteus will detect if the same type is defined in different ways and will throw an error showing precisely what the conflicts are if so.
You can call `findConflicts` to get the list of conflicts without throwing an error.
:::

`render` and `renderToFile` accept a list of `ProtoIR.TopLevelOption`, which are then added at the top of the .proto file.
```scala
val options = List(
  ProtoIR.TopLevelOption("java_package", "com.example")
)
println(entities.render(options))
// syntax = "proto3";
// 
// package com.example;
// 
// option java_package = "com.example";
// 
// ...
```

## Services

Similarly, you can render entire gRPC services.

```scala
import proteus.*

given ProtobufDeriver = ProtobufDeriver // your deriver instance

case class HelloRequest(name: String) derives ProtobufCodec
case class HelloResponse(message: String) derives ProtobufCodec

val helloRpc = Rpc.unary[HelloRequest, HelloResponse]("Hello")
val helloService = Service("examples", "Greeter").rpc(helloRpc)
println(helloService.render(Nil))
// syntax = "proto3";
// 
// package examples;
// 
// service Greeter {
//     rpc Hello (HelloRequest) returns (HelloResponse) {}
// }
// 
// message HelloRequest {
//     string name = 1;
// }
// 
// message HelloResponse {
//     string message = 1;
// }
// 
```
Services can also have dependencies via `dependsOn`.

::: tip
If you have multiple services, the `Dependency.fromServices` method will create a `Dependency` that contains all the types used in the services except for the request and response types of the RPCs.
That way, you can share the same types between services.
:::

## Build tool integration

A typical workflow is to add a task in your build tool to generate the `.proto` files.
For example, with sbt, you can add the following task where `examples.Protogen` is the main class that contains the code to generate the `.proto` files.
```scala
lazy val generateProtos = taskKey[Unit]("Generate proto files")

generateProtos := (Compile / runMain).toTask(" examples.Protogen").value
```

Then, you can run the task to generate the `.proto` files:
```bash
sbt generateProtos
```
You can even run this in CI, either to generate the `.proto` files and push them, or just to check that the `.proto` files are up to date.