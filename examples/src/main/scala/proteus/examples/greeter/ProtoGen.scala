package proteus.examples.greeter

import proteus.*

object ProtoGen {
  def main(args: Array[String]): Unit =
    greeterService.renderToFile(Nil, "examples/src/main/proto")
}
