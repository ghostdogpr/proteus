package proteus.examples.greeter

import zio.blocks.schema.Schema

import proteus.*

case class HelloRequest(name: String) derives Schema, ProtobufCodec
case class HelloReply(message: String) derives Schema, ProtobufCodec

given ProtobufDeriver = ProtobufDeriver

val sayHelloRpc = Rpc.unary[HelloRequest, HelloReply]("SayHello")

val greeterService = Service("examples", "Greeter").rpc(sayHelloRpc)
