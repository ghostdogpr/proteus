package proteus.examples.routeguide.kyo

import java.util.concurrent.TimeUnit

import io.grpc.{ManagedChannel, ManagedChannelBuilder, StatusException}
import kyo.*

import proteus.client.KyoClientBackend
import proteus.examples.routeguide.*

class RouteGuideClient(host: String, port: Int) {
  private def withChannel[A](f: ManagedChannel => A < (Async & Abort[StatusException])): A < (Async & Abort[StatusException]) =
    Sync.defer(ManagedChannelBuilder.forAddress(host, port).usePlaintext().build()).map { channel =>
      Sync.ensure(Sync.defer { channel.shutdown().awaitTermination(5, TimeUnit.SECONDS); () })(f(channel))
    }

  def getFeature(point: Point): Feature < (Async & Abort[StatusException]) =
    withChannel(channel => KyoClientBackend(channel).client(getFeatureRpc, routeGuideService)(point))

  def listFeatures(rectangle: Rectangle): Chunk[Feature] < (Async & Abort[StatusException]) =
    withChannel(channel => KyoClientBackend(channel).client(listFeaturesRpc, routeGuideService)(rectangle).run)

  def recordRoute(points: List[Point]): RouteSummary < (Async & Abort[StatusException]) =
    withChannel(channel => KyoClientBackend(channel).client(recordRouteRpc, routeGuideService)(Stream.init(points)))

  def routeChat(notes: List[RouteNote]): Chunk[RouteNote] < (Async & Abort[StatusException]) =
    withChannel(channel => KyoClientBackend(channel).client(routeChatRpc, routeGuideService)(Stream.init(notes)).run)

  def runDemo: Unit < (Async & Abort[StatusException]) =
    for {
      feature   <- getFeature(Point(409146138, -746188906))
      _         <- Console.printLine(if (feature.name.nonEmpty) s"Found feature: ${feature.name}" else "No feature found")
      features  <- listFeatures(Rectangle(Point(400000000, -750000000), Point(420000000, -730000000)))
      _         <- Console.printLine(s"Listed ${features.length} features")
      summary   <- recordRoute(List(Point(407838351, -746143763), Point(408122808, -743999179)))
      _         <- Console.printLine(s"Recorded route: ${summary.pointCount} points, ${summary.distance}m")
      responses <- routeChat(
                     List(
                       RouteNote(Point(0, 1), "First message at (0,1)"),
                       RouteNote(Point(0, 2), "Message at (0,2)"),
                       RouteNote(Point(0, 1), "Second message at (0,1)")
                     )
                   )
      _         <- Console.printLine(s"Route chat: received ${responses.length} responses")
    } yield ()
}
