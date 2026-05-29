package proteus.examples.routeguide.kyo

import kyo.*

import proteus.examples.routeguide.*

object RouteGuideExample extends KyoApp {
  run {
    for {
      _ <- Console.printLine("=== Proteus Route Guide Example ===")
      _ <- Console.printLine(routeGuideService.render(Nil))

      routeNotes <- AtomicRef.init(Map.empty[Point, List[RouteNote]])

      server = RouteGuideServer(8980, routeNotes)
      client = RouteGuideClient("localhost", 8980)

      _ <- Scope.run {
             server.serverResource
               .andThen(Console.printLine("Running Route Guide demo..."))
               .andThen(client.runDemo)
           }
    } yield ()
  }
}
