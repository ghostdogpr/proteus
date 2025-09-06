package proteus.examples.routeguide

import java.util.concurrent.TimeUnit

import io.grpc.ServerBuilder
import io.grpc.StatusException
import zio.*
import zio.stream.*

import proteus.server.{ServerService, ZioServerBackend}

class RouteGuideServer(port: Int, routeNotes: Ref[Map[Point, List[RouteNote]]]) {
  def getFeature(point: Point): UIO[Feature] =
    ZIO
      .log(s"Server: GetFeature(${point.latitude}, ${point.longitude})")
      .as(RouteGuideData.findFeature(point))

  def listFeatures(rectangle: Rectangle): ZStream[Any, Nothing, Feature] =
    ZStream.log(s"Server: ListFeatures") *>
      ZStream.fromIterable(RouteGuideData.findFeaturesInRectangle(rectangle))

  def recordRoute(points: ZStream[Any, StatusException, Point]): IO[StatusException, RouteSummary] =
    for {
      _                         <- ZIO.log(s"Server: RecordRoute")
      startTime                 <- Clock.currentTime(TimeUnit.MILLISECONDS)
      (pointCount, distance, _) <- points
                                     .runFold((0, 0, Option.empty[Point])) { case ((pointCount, distance, lastPoint), point) =>
                                       val newDistance = lastPoint.map(RouteGuideData.calcDistance(_, point)).getOrElse(0) + distance
                                       (pointCount + 1, newDistance, Some(point))
                                     }
      endTime                   <- Clock.currentTime(TimeUnit.MILLISECONDS)
      elapsedTime                = ((endTime - startTime) / 1000).toInt
    } yield RouteSummary(pointCount, 0, distance, elapsedTime)

  def routeChat(notes: ZStream[Any, StatusException, RouteNote]): ZStream[Any, StatusException, RouteNote] =
    notes.mapZIO { note =>
      for {
        oldMap       <- routeNotes.getAndUpdate(map => map + (note.location -> (note :: map.getOrElse(note.location, List.empty))))
        existingNotes = oldMap.getOrElse(note.location, List.empty)
      } yield ZStream.fromIterable(existingNotes)
    }.flatten

  val service = ServerService(using ZioServerBackend)
    .rpc(getFeatureRpc, getFeature)
    .rpc(listFeaturesRpc, listFeatures)
    .rpc(recordRouteRpc, recordRoute)
    .rpc(routeChatRpc, routeChat)
    .build(routeGuideService)

  val server = ServerBuilder.forPort(port).addService(service).build()

  val start: Task[Unit] =
    ZIO.attempt {
      server.start()
      sys.addShutdownHook(unsafeStop())
    } *> ZIO.log(s"Server started on port $port")

  val stop: Task[Unit] =
    ZIO.attemptBlocking {
      unsafeStop()
    }

  private def unsafeStop(): Unit =
    server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
}
