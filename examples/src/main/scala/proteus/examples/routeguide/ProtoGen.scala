package proteus.examples.routeguide

import proteus.*

object ProtoGen {
  def main(args: Array[String]): Unit =
    routeGuideService.renderToFile(Nil, "examples/src/main/proto")
}
