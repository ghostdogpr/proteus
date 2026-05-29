package proteus.examples.routeguide.kyo

import java.util.concurrent.atomic.AtomicReference

import io.grpc.StatusException
import kyo.*

import proteus.examples.routeguide.*
import proteus.server.{KyoServerBackend, ServerService}

class RouteGuideServer(port: Int) {
  private val routeNotes = new AtomicReference[Map[Point, List[RouteNote]]](Map.empty)

  def getFeature(point: Point): Feature < (Async & Abort[StatusException]) =
    Console.printLine(s"Server: GetFeature(${point.latitude}, ${point.longitude})").andThen(RouteGuideData.findFeature(point))

  def listFeatures(rectangle: Rectangle): Stream[Feature, Async & Abort[StatusException]] =
    Stream.init(RouteGuideData.findFeaturesInRectangle(rectangle))

  def recordRoute(points: Stream[Point, Async & Abort[StatusException]]): RouteSummary < (Async & Abort[StatusException]) =
    points
      .fold((0, 0, Option.empty[Point])) { case ((count, distance, last), point) =>
        val added = last.fold(0)(lp => RouteGuideData.calcDistance(lp, point))
        (count + 1, distance + added, Some(point))
      }
      .map { case (count, distance, _) => RouteSummary(count, 0, distance, 0) }

  def routeChat(notes: Stream[RouteNote, Async & Abort[StatusException]]): Stream[RouteNote, Async & Abort[StatusException]] =
    notes.mapChunk { chunk =>
      chunk.flatMap { note =>
        val previous = routeNotes.getAndUpdate(map => map + (note.location -> (note :: map.getOrElse(note.location, List.empty))))
        previous.getOrElse(note.location, List.empty)
      }
    }

  val backend = KyoServerBackend()

  val service = ServerService(using backend)
    .rpc(getFeatureRpc, getFeature)
    .rpc(listFeaturesRpc, listFeatures)
    .rpc(recordRouteRpc, recordRoute)
    .rpc(routeChatRpc, routeChat)
    .build(routeGuideService)

  def start: io.grpc.Server < Sync =
    Sync.defer(io.grpc.ServerBuilder.forPort(port).addService(service).build().start()).map { server =>
      Console.printLine(s"Server started on port $port").andThen(server)
    }
}
