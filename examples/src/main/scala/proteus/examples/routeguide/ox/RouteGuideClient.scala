package proteus.examples.routeguide.ox

import java.util.concurrent.TimeUnit

import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import ox.flow.Flow

import proteus.client.OxClientBackend
import proteus.examples.routeguide.*

class RouteGuideClient(host: String, port: Int) {
  private def withChannel[T](f: ManagedChannel => T): T = {
    val channel = ManagedChannelBuilder.forAddress(host, port).usePlaintext().build()
    try f(channel)
    finally channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
  }

  def getFeature(point: Point): Feature =
    withChannel { channel =>
      val backend = new OxClientBackend(channel)
      val client  = backend.client(getFeatureRpc, routeGuideService)
      client(point)
    }

  def listFeatures(rectangle: Rectangle): List[Feature] =
    withChannel { channel =>
      val backend = new OxClientBackend(channel)
      val client  = backend.client(listFeaturesRpc, routeGuideService)
      client(rectangle).runToList()
    }

  def recordRoute(points: List[Point]): RouteSummary =
    withChannel { channel =>
      val backend = new OxClientBackend(channel)
      val client  = backend.client(recordRouteRpc, routeGuideService)
      client(Flow.fromIterable(points))
    }

  def routeChat(): List[RouteNote] = {
    val notes = List(
      RouteNote(Point(0, 1), "First message at (0,1)"),
      RouteNote(Point(0, 2), "Message at (0,2)"),
      RouteNote(Point(0, 1), "Second message at (0,1)"),
      RouteNote(Point(0, 1), "Third message at (0,1)")
    )

    withChannel { channel =>
      val backend = new OxClientBackend(channel)
      val client  = backend.client(routeChatRpc, routeGuideService)
      client(Flow.fromIterable(notes)).runToList()
    }
  }

  def runDemo(): Unit = {
    val feature = getFeature(Point(409146138, -746188906))
    println(if (feature.name.nonEmpty) s"Found feature: ${feature.name}" else "No feature found")

    val rectangle = Rectangle(Point(400000000, -750000000), Point(420000000, -730000000))
    val features  = listFeatures(rectangle)
    println(s"Listed ${features.length} features")

    val points  = List(Point(407838351, -746143763), Point(408122808, -743999179))
    val summary = recordRoute(points)
    println(s"Recorded route: ${summary.pointCount} points, ${summary.distance}m")

    val responses = routeChat()
    println(s"Route chat: received ${responses.length} responses")
  }
}
