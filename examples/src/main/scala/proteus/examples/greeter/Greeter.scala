package proteus.examples.greeter

import zio.blocks.schema.Schema

import proteus.*

case class HelloRequest(name: String) derives Schema
case class HelloReply(message: String) derives Schema

given ProtobufDeriver = ProtobufDeriver()

val sayHelloRpc = Rpc.unary[HelloRequest, HelloReply]("SayHello")

val greeterService = Service("examples.greeter", "Greeter").rpc(sayHelloRpc)
