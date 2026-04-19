# gRPC services

Proteus allows you to define RPCs and gRPC services in Scala using your own types, relying on `ProtobufCodec` to encode and decode the messages.

It uses [grpc-java](https://github.com/grpc/grpc-java) under the hood, either directly (direct style and `Future` backends) or via the [zio-grpc](https://github.com/scalapb/zio-grpc), [fs2-grpc](https://github.com/typelevel/fs2-grpc), or [Ox](https://ox.softwaremill.com) libraries.

In order to use it, you will need to add the following dependency to your `build.sbt` file:

```scala
"com.github.ghostdogpr" %% "proteus-grpc" % "@VERSION@"
// optional, only if you use these backends
"com.github.ghostdogpr" %% "proteus-grpc-zio" % "@VERSION@" // zio backend
"com.github.ghostdogpr" %% "proteus-grpc-fs2" % "@VERSION@" // fs2 backend
"com.github.ghostdogpr" %% "proteus-grpc-ox"  % "@VERSION@" // ox backend (requires JDK 21+)
```

## Defining services

To define an RPC, you first need to define two case classes, one for the request and one for the response.
These case classes require a `ProtobufCodec` instance, which can be derived automatically using the `derives` keyword provided that a `given ProtobufDeriver` instance is in scope. Codecs for types used in the request and response will be derived automatically as well.
```scala
given ProtobufDeriver = ProtobufDeriver // your deriver instance

case class HelloRequest(name: String) derives ProtobufCodec
case class HelloReply(message: String) derives ProtobufCodec
```

Then, you can define the RPC using the `Rpc` case class.
```scala
val sayHelloRpc = Rpc.unary[HelloRequest, HelloReply]("SayHello")
```

The `Rpc` case class has four constructors, corresponding to the four streaming patterns: `unary`, `clientStreaming`, `serverStreaming`, and `bidiStreaming`.

To define a service, you need to provide a package name and the name of the service. Then, you can add the RPCs it contains using the `rpc` method for each of them.
```scala
val greeterService = Service("examples", "Greeter").rpc(sayHelloRpc)
```

## Server implementation

To turn this service into a server, you need to provide a backend and the logic for each of the RPCs.

```scala
import proteus.server.*

val service = 
  ServerService(using DirectServerBackend)
    .rpc(sayHelloRpc, request => HelloReply(s"Hello, ${request.name}!"))
    .build(greeterService)
```

The expected type for the logic function depends on two things: the RPC type and the backend. It has the following shape:
- Unary: `Request => Unary[Response]`
- Client Streaming: `Streaming[Request] => Unary[Response]`
- Server Streaming: `Request => Streaming[Response]`
- Bidirectional Streaming: `Streaming[Request] => Streaming[Response]`

Here are the values for the different backends:
- `DirectServerBackend`: omit the `Unary` wrapper, so the logic function is just `Request => Response`. Streaming is not supported.
- `FutureServerBackend`: `Unary` is `Future`. Streaming is not supported.
- `ZioServerBackend`: `Unary` is `ZIO[R, E, *]`, `Streaming` is `ZStream[R, E, *]`.
- `Fs2ServerBackend`: `Unary` is `F`, `Streaming` is `Stream[F, *]`.
- `OxServerBackend`: omit the `Unary` wrapper (direct style), `Streaming` is `Flow[*]`.

It is possible to implement your own custom backend quite easily as long as you are using grpc-java under the hood.

::: tip
The order in which you call the `rpc` method does not matter. But you will get a compile error if you call `build` before providing the logic for all of the RPCs.
:::

The obtained `service` is a `ServerServiceDefinition` that can be used to start a gRPC server.

```scala
import io.grpc.ServerBuilder

ServerBuilder.forPort(8080).addService(service).build().start()
```

### Context

In addition to the `.rpc` method, you can also use `.rpcWithContext` if you want to receive the context of the call.
In this case, the logic function has the following shape: `(Request, Context) => Unary[Response]`, `(Streaming[Request], Context) => Unary[Response]`, etc.

The default `Context` is `proteus.server.RequestResponseMetadata`, a type that contains request and response metadata, except for the ZIO backend, which uses `scalapb.zio_grpc.RequestContext` instead.

```scala
def processHello(request: HelloRequest, ctx: RequestResponseMetadata): HelloReply = {
  println(s"Request metadata: ${ctx.requestMetadata}")
  HelloReply(s"Hello, ${request.name}!")
}

val service = 
  ServerService(using DirectServerBackend)
    .rpcWithContext(sayHelloRpc, processHello)
    .build(greeterService)
```

### Interceptors

Interceptors allow you to change the `Context` type or the `Unary`/`Streaming` wrapper types you use in your logic function. They run on every request and can also be used to run additional logic before or after the request is processed.

There are two traits you can implement to create an interceptor: `ServerInterceptor` and `ServerContextInterceptor`.
The first one is a more general trait that can transform the context and the wrapper types for unary and streaming RPCs.
The second one is a more specific trait that can only transform the context.

Let's look at an example:
```scala
val interceptor = 
  new ServerContextInterceptor[[A] =>> A, [A] =>> A, RequestResponseMetadata, String] {
    def transformContext(context: RequestResponseMetadata): String =
      context.requestMetadata.get(Metadata.Key.of("auth-token", Metadata.ASCII_STRING_MARSHALLER))
  }
```
This interceptor changes the `Context` type from initial `RequestResponseMetadata` to `String` for the direct backend.
On every request, it will extract the `auth-token` from the request metadata and pass it to the logic function. Your logic function just needs to be `(Request, String) => Response`.

::: tip
In the example, `[A] =>> A` is the identity type constructor, which is the value of `Unary` and `Streaming` for the direct backend. It means that `Response` is not wrapped in any monad.
:::

To use the interceptor, you just pass it when creating the backend:
```scala
val backend = DirectServerBackend(interceptor)
val service = ServerService(using backend) //.rpc(...).build(...)
```

## Client implementation

Creating a client is similar to creating a server, except you don't need to provide any logic, only a backend.

Creating a backend requires a `Channel` instance, which can be created using `ManagedChannelBuilder`.
Then you call `backend.client` to create a client for the given RPC and service.

```scala
import io.grpc.*
import proteus.client.*

val channel = ManagedChannelBuilder
  .forAddress("localhost", 8080)
  .usePlaintext()
  .build()
val backend = DirectClientBackend(channel)
val sayHelloClient = backend.client(sayHelloRpc, greeterService)
```
Once again, the return type of `client` depends on the backend you are using:
- Unary: `Unary[Request => Unary[Response]]`
- Client Streaming: `Unary[Streaming[Request] => Unary[Response]]`
- Server Streaming: `Unary[Request => Streaming[Response]]`
- Bidirectional Streaming: `Unary[Streaming[Request] => Streaming[Response]]`

So with our direct backend, the return type is `Request => Response`.
```scala
sayHelloClient(HelloRequest("Pierre"))
// HelloReply("Hello, Pierre!")
```

### Metadata

There is also a variant of `client` that allows you to send and receive metadata. It is called `clientWithMetadata` and returns a function that has the following shape:
- Unary: `Unary[(Request, Metadata) => Unary[(Response, Metadata)]]`
- Client Streaming: `Unary[(Streaming[Request], Metadata) => Unary[(Response, Metadata)]]`
- Server Streaming: `Unary[(Request, Metadata) => Streaming[Response]]`
- Bidirectional Streaming: `Unary[(Streaming[Request], Metadata) => Streaming[Response]]`

So with our direct backend, the return type is `(Request, Metadata) => (Response, Metadata)`.
```scala
val sayHelloClientWithMetadata = backend.clientWithMetadata(sayHelloRpc, greeterService)

val requestMetadata = new Metadata()
requestMetadata.put(Metadata.Key.of("auth-token", Metadata.ASCII_STRING_MARSHALLER), "1234567890")
sayHelloClientWithMetadata(HelloRequest("Pierre"), requestMetadata)._1
// HelloReply("Hello, Pierre!")
```