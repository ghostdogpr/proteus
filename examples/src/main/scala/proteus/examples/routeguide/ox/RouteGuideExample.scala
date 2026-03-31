package proteus.examples.routeguide.ox

import java.util.concurrent.TimeUnit

import ox.{inScopeRunner, supervised}

import proteus.examples.routeguide.*

object RouteGuideExample {
  def main(args: Array[String]): Unit = {
    println("=== Proteus Route Guide Example (Ox) ===")
    println(routeGuideService.render(Nil))

    supervised {
      val server     = RouteGuideServer(8982, inScopeRunner())
      val grpcServer = server.start()

      try {
        println("Running Route Guide demo...")
        val client = RouteGuideClient("localhost", 8982)
        client.runDemo()
      } finally
        grpcServer.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
    }
  }
}
