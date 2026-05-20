package proteus.examples.routeguide.fs2

import java.util.concurrent.TimeUnit

import cats.effect.{IO, Resource}
import cats.effect.std.Dispatcher
import fs2.Stream
import io.grpc.{ManagedChannel, ManagedChannelBuilder}

import proteus.client.Fs2ClientBackend
import proteus.examples.routeguide.*

class RouteGuideClient(host: String, port: Int)(using dispatcher: Dispatcher[IO]) {
  val channelResource: Resource[IO, ManagedChannel] =
    Resource.make(
      IO.delay(ManagedChannelBuilder.forAddress(host, port).usePlaintext().build())
    )(channel => IO.blocking(channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit))

  def getFeature(point: Point): IO[Feature] =
    channelResource.use { channel =>
      val backend = Fs2ClientBackend[IO](channel, dispatcher)
      backend.client(getFeatureRpc, routeGuideService)(point)
    }

  def listFeatures(rectangle: Rectangle): IO[List[Feature]] =
    channelResource.use { channel =>
      val backend = Fs2ClientBackend[IO](channel, dispatcher)
      backend.client(listFeaturesRpc, routeGuideService)(rectangle).compile.toList
    }

  def recordRoute(points: List[Point]): IO[RouteSummary] =
    channelResource.use { channel =>
      val backend = Fs2ClientBackend[IO](channel, dispatcher)
      backend.client(recordRouteRpc, routeGuideService)(Stream.emits(points))
    }

  def routeChat(): IO[List[RouteNote]] = {
    val notes = List(
      RouteNote(Point(0, 1), "First message at (0,1)"),
      RouteNote(Point(0, 2), "Message at (0,2)"),
      RouteNote(Point(0, 1), "Second message at (0,1)"),
      RouteNote(Point(0, 1), "Third message at (0,1)")
    )

    channelResource.use { channel =>
      val backend = Fs2ClientBackend[IO](channel, dispatcher)
      backend.client(routeChatRpc, routeGuideService)(Stream.emits(notes)).compile.toList
    }
  }

  val runDemo: IO[Unit] =
    for {
      feature <- getFeature(Point(409146138, -746188906))
      _       <- IO.println(if (feature.name.nonEmpty) s"Found feature: ${feature.name}" else "No feature found")

      rectangle = Rectangle(Point(400000000, -750000000), Point(420000000, -730000000))
      features <- listFeatures(rectangle)
      _        <- IO.println(s"Listed ${features.length} features")

      points   = List(Point(407838351, -746143763), Point(408122808, -743999179))
      summary <- recordRoute(points)
      _       <- IO.println(s"Recorded route: ${summary.pointCount} points, ${summary.distance}m")

      responses <- routeChat()
      _         <- IO.println(s"Route chat: received ${responses.length} responses")
    } yield ()

}
