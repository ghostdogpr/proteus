package proteus.examples.greeter

import proteus.*

object GreeterExample {

  @main
  def main: Unit = {
    println("=== Proteus Greeter Example ===")
    println(greeterService.render(Nil))
    println()

    val server = GreeterServer(50051)
    val client = GreeterClient("localhost", 50051)

    try {
      server.start()

      println(s"${client.sayHello("World").message}")
      println(s"${client.sayHello("Proteus").message}")

    } finally {
      client.shutdown()
      server.stop()
    }
  }
}
