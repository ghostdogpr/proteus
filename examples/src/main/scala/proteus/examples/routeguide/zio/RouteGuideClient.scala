package proteus.examples.routeguide.zio

import java.util.concurrent.TimeUnit

import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import zio.*
import zio.stream.*

import proteus.client.ZioClientBackend
import proteus.examples.routeguide.*

class RouteGuideClient(host: String, port: Int) {
  val channelResource: ZIO[Scope, Throwable, ManagedChannel] =
    ZIO.acquireRelease(
      ZIO.attempt(ManagedChannelBuilder.forAddress(host, port).usePlaintext().build())
    )(channel => ZIO.attemptBlocking(channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)).ignoreLogged)

  def getFeature(point: Point): Task[Feature] =
    ZIO.scoped {
      for {
        channel <- channelResource
        backend  = new ZioClientBackend(channel)
        result  <- backend.client(getFeatureRpc, routeGuideService)(point)
      } yield result
    }

  def listFeatures(rectangle: Rectangle): Task[List[Feature]] =
    ZIO.scoped {
      for {
        channel  <- channelResource
        backend   = new ZioClientBackend(channel)
        features <- backend.client(listFeaturesRpc, routeGuideService)(rectangle).runCollect
      } yield features.toList
    }

  def recordRoute(points: List[Point]): Task[RouteSummary] =
    ZIO.scoped {
      for {
        channel <- channelResource
        backend  = new ZioClientBackend(channel)
        result  <- backend.client(recordRouteRpc, routeGuideService)(ZStream.fromIterable(points))
      } yield result
    }

  def routeChat(): Task[List[RouteNote]] = {
    val notes = List(
      RouteNote(Point(0, 1), "First message at (0,1)"),
      RouteNote(Point(0, 2), "Message at (0,2)"),
      RouteNote(Point(0, 1), "Second message at (0,1)"),
      RouteNote(Point(0, 1), "Third message at (0,1)")
    )

    ZIO.scoped {
      for {
        channel   <- channelResource
        backend    = new ZioClientBackend(channel)
        responses <- backend.client(routeChatRpc, routeGuideService)(ZStream.fromIterable(notes)).runCollect
      } yield responses.toList
    }
  }

  val runDemo: Task[Unit] =
    for {
      feature <- getFeature(Point(409146138, -746188906))
      _       <- ZIO.log(if (feature.name.nonEmpty) s"Found feature: ${feature.name}" else "No feature found")

      rectangle = Rectangle(Point(400000000, -750000000), Point(420000000, -730000000))
      features <- listFeatures(rectangle)
      _        <- ZIO.log(s"Listed ${features.length} features")

      points   = List(Point(407838351, -746143763), Point(408122808, -743999179))
      summary <- recordRoute(points)
      _       <- ZIO.log(s"Recorded route: ${summary.pointCount} points, ${summary.distance}m")

      responses <- routeChat()
      _         <- ZIO.log(s"Route chat: received ${responses.length} responses")
    } yield ()

}
