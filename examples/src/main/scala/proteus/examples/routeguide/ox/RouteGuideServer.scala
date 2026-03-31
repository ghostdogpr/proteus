package proteus.examples.routeguide.ox

import java.util.concurrent.atomic.AtomicReference

import ox.InScopeRunner
import ox.flow.Flow

import proteus.examples.routeguide.*
import proteus.server.{OxServerBackend, ServerService}

class RouteGuideServer(port: Int, runner: InScopeRunner) {
  private val routeNotes = new AtomicReference[Map[Point, List[RouteNote]]](Map.empty)

  def getFeature(point: Point): Feature = {
    println(s"Server: GetFeature(${point.latitude}, ${point.longitude})")
    RouteGuideData.findFeature(point)
  }

  def listFeatures(rectangle: Rectangle): Flow[Feature] = {
    println("Server: ListFeatures")
    Flow.fromIterable(RouteGuideData.findFeaturesInRectangle(rectangle))
  }

  def recordRoute(points: Flow[Point]): RouteSummary = {
    println("Server: RecordRoute")
    val startTime                = System.currentTimeMillis()
    var pointCount               = 0
    var distance                 = 0
    var lastPoint: Option[Point] = None

    points.runForeach { point =>
      pointCount += 1
      lastPoint.foreach(lp => distance += RouteGuideData.calcDistance(lp, point))
      lastPoint = Some(point)
    }

    val elapsedTime = ((System.currentTimeMillis() - startTime) / 1000).toInt
    RouteSummary(pointCount, 0, distance, elapsedTime)
  }

  def routeChat(notes: Flow[RouteNote]): Flow[RouteNote] =
    notes.mapConcat { note =>
      val oldMap = routeNotes.getAndUpdate(map => map + (note.location -> (note :: map.getOrElse(note.location, List.empty))))
      oldMap.getOrElse(note.location, List.empty)
    }

  val backend = OxServerBackend(runner)

  val service = ServerService(using backend)
    .rpc(getFeatureRpc, getFeature)
    .rpc(listFeaturesRpc, listFeatures)
    .rpc(recordRouteRpc, recordRoute)
    .rpc(routeChatRpc, routeChat)
    .build(routeGuideService)

  def start(): io.grpc.Server = {
    val server = io.grpc.ServerBuilder.forPort(port).addService(service).build()
    server.start()
    println(s"Server started on port $port")
    server
  }
}
