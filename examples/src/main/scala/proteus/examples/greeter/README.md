# Greeter Example

The classic "Hello World" gRPC example using Proteus.

## Running

```bash
sbt "examples/runMain proteus.examples.greeter.GreeterExample"
```

## Code Structure

**Messages & Service** (`Greeter.scala`):
```scala
case class HelloRequest(name: String) derives ProtobufCodec
case class HelloReply(message: String) derives ProtobufCodec

val sayHelloRpc = Rpc.unary[HelloRequest, HelloReply]("SayHello")

val greeterService = Service("examples", "Greeter").rpc(sayHelloRpc)
```

**Server** (`GreeterServer.scala`):
```scala
class GreeterServer(port: Int) {
  val service = ServerService(using DirectServerBackend)
    .rpc(sayHelloRpc, sayHello)
    .build(greeterService)

  val server = ServerBuilder.forPort(port).addService(service).build()
}
```

**Client** (`GreeterClient.scala`):
```scala
class GreeterClient(host: String, port: Int) {
  val channel: ManagedChannel = ManagedChannelBuilder.forAddress(host, port).usePlaintext().build()
  val backend                 = DirectClientBackend(channel)
  val sayHelloClient          = backend.client(greeterService, sayHelloRpc)
}
```

## Generated Protobuf

```protobuf
syntax = "proto3";

package examples;

service Greeter {
    rpc SayHello (HelloRequest) returns (HelloReply) {}
}

message HelloRequest {
    string name = 1;
}

message HelloReply {
    string message = 1;
}
```