package proteus.examples.routeguide.kyo

import java.util.concurrent.TimeUnit

import io.grpc.ServerBuilder
import io.grpc.StatusException
import kyo.*

import proteus.examples.routeguide.*
import proteus.server.{KyoServerBackend, ServerService}

class RouteGuideServer(port: Int, routeNotes: AtomicRef[Map[Point, List[RouteNote]]]) {
  def getFeature(point: Point): Feature < (Async & Abort[StatusException]) =
    Console
      .printLine(s"Server: GetFeature(${point.latitude}, ${point.longitude})")
      .andThen(RouteGuideData.findFeature(point))

  def listFeatures(rectangle: Rectangle): Stream[Feature, Async & Abort[StatusException]] =
    Stream.unwrap {
      Console.printLine("Server: ListFeatures").andThen(Stream.init(RouteGuideData.findFeaturesInRectangle(rectangle)))
    }

  def recordRoute(points: Stream[Point, Async & Abort[StatusException]]): RouteSummary < (Async & Abort[StatusException]) =
    for {
      _                        <- Console.printLine("Server: RecordRoute")
      startTime                <- Clock.now
      res                      <- points
                                    .fold((0, 0, Option.empty[Point])) { case ((pointCount, distance, lastPoint), point) =>
                                      val newDistance = lastPoint.map(RouteGuideData.calcDistance(_, point)).getOrElse(0) + distance
                                      (pointCount + 1, newDistance, Some(point))
                                    }
      (pointCount, distance, _) = res
      endTime                  <- Clock.now
      elapsedTime               = (endTime - startTime).toSeconds.toInt
    } yield RouteSummary(pointCount, 0, distance, elapsedTime)

  def routeChat(notes: Stream[RouteNote, Async & Abort[StatusException]]): Stream[RouteNote, Async & Abort[StatusException]] =
    notes.flatMap { note =>
      for {
        oldMap       <- routeNotes.getAndUpdate(map => map + (note.location -> (note :: map.getOrElse(note.location, List.empty))))
        existingNotes = oldMap.getOrElse(note.location, List.empty)
      } yield Stream.init(existingNotes)
    }

  val service = ServerService(using KyoServerBackend())
    .rpc(getFeatureRpc, getFeature)
    .rpc(listFeaturesRpc, listFeatures)
    .rpc(recordRouteRpc, recordRoute)
    .rpc(routeChatRpc, routeChat)
    .build(routeGuideService)

  val serverResource: io.grpc.Server < (Scope & Async) =
    Scope.acquireRelease(
      Sync
        .defer {
          val server = ServerBuilder.forPort(port).addService(service).build()
          server.start()
          server
        }
        .map(server => Console.printLine(s"Server started on port $port").andThen(server))
    )(server => Sync.defer(server.shutdown().awaitTermination(5, TimeUnit.SECONDS)))
}
