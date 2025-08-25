package proteus.examples.greeter

import java.util.concurrent.TimeUnit

import io.grpc.{ManagedChannel, ManagedChannelBuilder}

import proteus.client.DirectClientBackend

class GreeterClient(host: String, port: Int) {
  private val channel: ManagedChannel = ManagedChannelBuilder.forAddress(host, port).usePlaintext().build()
  private val backend                 = DirectClientBackend(channel)
  private val sayHelloClient          = backend.client(greeterService, sayHelloRpc)

  def sayHello(name: String): HelloReply = {
    println(s"Client: SayHello($name)")
    sayHelloClient(HelloRequest(name))
  }

  def shutdown(): Unit = {
    channel.shutdown()
    channel.awaitTermination(5, TimeUnit.SECONDS): Unit
  }
}
