package proteus.examples.greeter

import proteus.*

given ProtobufDeriver = ProtobufDeriver

case class HelloRequest(name: String) derives ProtobufCodec
case class HelloReply(message: String) derives ProtobufCodec

val sayHelloRpc = Rpc.unary[HelloRequest, HelloReply]("SayHello")

val greeterService = Service("examples", "Greeter").rpc(sayHelloRpc)
