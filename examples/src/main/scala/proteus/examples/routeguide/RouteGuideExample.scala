package proteus.examples.routeguide

import zio.*

import proteus.*

object RouteGuideExample extends ZIOAppDefault {

  val run =
    for {
      _ <- ZIO.log("=== Proteus Route Guide Example ===")
      _ <- ZIO.log(routeGuideService.render(Nil))

      routeNotes <- Ref.make(Map.empty[Point, List[RouteNote]])

      server = RouteGuideServer(8980, routeNotes)
      client = RouteGuideClient("localhost", 8980)

      _ <- server.start
      _ <- ZIO.log("Running Route Guide demo...")
      _ <- client.runDemo
      _ <- client.shutdown
      _ <- server.stop
    } yield ()
}
