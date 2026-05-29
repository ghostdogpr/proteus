package proteus.examples.routeguide.kyo

import java.util.concurrent.TimeUnit

import io.grpc.{ManagedChannel, ManagedChannelBuilder, StatusException}
import kyo.*

import proteus.client.KyoClientBackend
import proteus.examples.routeguide.*

class RouteGuideClient(host: String, port: Int) {
  val channelResource: ManagedChannel < (Scope & Async) =
    Scope.acquireRelease(
      Sync.defer(ManagedChannelBuilder.forAddress(host, port).usePlaintext().build())
    )(channel => Sync.defer { channel.shutdown().awaitTermination(5, TimeUnit.SECONDS); () })

  def getFeature(point: Point): Feature < (Async & Abort[StatusException]) =
    Scope.run {
      for {
        channel <- channelResource
        backend  = KyoClientBackend(channel)
        result  <- backend.client(getFeatureRpc, routeGuideService)(point)
      } yield result
    }

  def listFeatures(rectangle: Rectangle): List[Feature] < (Async & Abort[StatusException]) =
    Scope.run {
      for {
        channel  <- channelResource
        backend   = KyoClientBackend(channel)
        features <- backend.client(listFeaturesRpc, routeGuideService)(rectangle).run
      } yield features.toList
    }

  def recordRoute(points: List[Point]): RouteSummary < (Async & Abort[StatusException]) =
    Scope.run {
      for {
        channel <- channelResource
        backend  = KyoClientBackend(channel)
        result  <- backend.client(recordRouteRpc, routeGuideService)(Stream.init(points))
      } yield result
    }

  def routeChat(): List[RouteNote] < (Async & Abort[StatusException]) = {
    val notes = List(
      RouteNote(Point(0, 1), "First message at (0,1)"),
      RouteNote(Point(0, 2), "Message at (0,2)"),
      RouteNote(Point(0, 1), "Second message at (0,1)"),
      RouteNote(Point(0, 1), "Third message at (0,1)")
    )

    Scope.run {
      for {
        channel   <- channelResource
        backend    = KyoClientBackend(channel)
        responses <- backend.client(routeChatRpc, routeGuideService)(Stream.init(notes)).run
      } yield responses.toList
    }
  }

  val runDemo: Unit < (Async & Abort[StatusException]) =
    for {
      feature <- getFeature(Point(409146138, -746188906))
      _       <- Console.printLine(if (feature.name.nonEmpty) s"Found feature: ${feature.name}" else "No feature found")

      rectangle = Rectangle(Point(400000000, -750000000), Point(420000000, -730000000))
      features <- listFeatures(rectangle)
      _        <- Console.printLine(s"Listed ${features.length} features")

      points   = List(Point(407838351, -746143763), Point(408122808, -743999179))
      summary <- recordRoute(points)
      _       <- Console.printLine(s"Recorded route: ${summary.pointCount} points, ${summary.distance}m")

      responses <- routeChat()
      _         <- Console.printLine(s"Route chat: received ${responses.length} responses")
    } yield ()

}
