package proteus.examples.greeter

import java.util.concurrent.TimeUnit

import io.grpc.{Server, ServerBuilder}

import proteus.server.{DirectServerBackend, ServerServiceBuilder}

class GreeterServer(port: Int) {
  private def sayHello(request: HelloRequest): HelloReply = {
    println(s"Server: SayHello(${request.name})")
    HelloReply(s"Hello, ${request.name}!")
  }

  private val service =
    ServerServiceBuilder(using DirectServerBackend).rpc(sayHelloRpc, sayHello).build(greeterService).definition

  private val server: Server = ServerBuilder.forPort(port).addService(service).build()

  def start(): Unit = {
    server.start()
    println(s"Server started on port $port")
    sys.addShutdownHook(stop()): Unit
  }

  def stop(): Unit =
    server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
}
