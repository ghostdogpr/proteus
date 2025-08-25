# Greeter Example

The classic "Hello World" gRPC example using Proteus.

## Running

```bash
sbt "examples/runMain proteus.examples.greeter.GreeterExample"
```

## Code Structure

**Messages & Service** (`Greeter.scala`):
```scala
case class HelloRequest(name: String) derives Schema
case class HelloReply(message: String) derives Schema

val sayHelloRpc = Rpc.unary[HelloRequest, HelloReply]("SayHello")

val greeterService = Service("examples.greeter", "Greeter")
  .rpc(sayHelloRpc)
```

**Server** (`GreeterServer.scala`):
```scala
class GreeterServer(port: Int) {
  private val service = ServerServiceBuilder(using DirectServerBackend)
    .rpc(sayHelloRpc, sayHello)
    .build(greeterService).definition

  private val server = ServerBuilder.forPort(port).addService(service).build()
}
```

**Client** (`GreeterClient.scala`):
```scala
class GreeterClient(host: String, port: Int) {
  private val channel: ManagedChannel = ManagedChannelBuilder.forAddress(host, port).usePlaintext().build()
  private val backend                 = DirectClientBackend(channel)
  private val sayHelloClient          = backend.client(greeterService, sayHelloRpc)
}
```

## Generated Protobuf

```protobuf
syntax = "proto3";

package examples.greeter;

service Greeter {
    rpc SayHello(HelloRequest) returns (HelloReply);
}

message HelloRequest {
    string name = 1;
}

message HelloReply {
    string message = 1;
}
```