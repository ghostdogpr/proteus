package proteus.examples.routeguide

import java.util.concurrent.TimeUnit

import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import scalapb.zio_grpc.ZChannel
import zio.*
import zio.stream.*

import proteus.client.ZioClientBackend

class RouteGuideClient(host: String, port: Int) {
  val channel: ManagedChannel = ManagedChannelBuilder.forAddress(host, port).usePlaintext().build()
  val zChannel                = ZChannel(channel, Seq.empty)
  val backend                 = new ZioClientBackend(zChannel)

  def getFeature(point: Point): Task[Feature] =
    for {
      client <- backend.client(routeGuideService, getFeatureRpc)
      result <- client(point)
    } yield result

  def listFeatures(rectangle: Rectangle): Task[List[Feature]] =
    for {
      client   <- backend.client(routeGuideService, listFeaturesRpc)
      features <- client(rectangle).runCollect
    } yield features.toList

  def recordRoute(points: List[Point]): Task[RouteSummary] =
    for {
      client <- backend.client(routeGuideService, recordRouteRpc)
      result <- client(ZStream.fromIterable(points))
    } yield result

  def routeChat(): Task[List[RouteNote]] = {
    val notes = List(
      RouteNote(Point(0, 1), "Hello at (0,1)"),
      RouteNote(Point(0, 2), "Hello at (0,2)"),
      RouteNote(Point(1, 0), "Hello at (1,0)")
    )

    for {
      client    <- backend.client(routeGuideService, routeChatRpc)
      responses <- client(ZStream.fromIterable(notes)).runCollect
    } yield responses.toList
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

  val shutdown: Task[Unit] =
    ZIO.attemptBlocking {
      channel.shutdown()
      channel.awaitTermination(5, TimeUnit.SECONDS): Unit
    }
}
