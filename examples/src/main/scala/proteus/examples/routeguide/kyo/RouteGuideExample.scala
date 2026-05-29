package proteus.examples.routeguide.kyo

import java.util.concurrent.TimeUnit

import kyo.*

import proteus.examples.routeguide.*

object RouteGuideExample extends KyoApp {
  run {
    for {
      _ <- Console.printLine("=== Proteus Route Guide Example (Kyo) ===")
      _ <- Console.printLine(routeGuideService.render(Nil))
      _ <- Scope.acquireRelease(RouteGuideServer(8983).start)(server => Sync.defer { server.shutdown().awaitTermination(5, TimeUnit.SECONDS); () })
      _ <- Console.printLine("Running Route Guide demo...")
      _ <- RouteGuideClient("localhost", 8983).runDemo
    } yield ()
  }
}
