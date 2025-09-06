package proteus.examples.routeguide

import proteus.*

object ProtoGen extends App {
  routeGuideService.renderToFile(Nil, "examples/src/main/proto")
}
