# Route Guide Example

A comprehensive gRPC example demonstrating all four types of service methods using Proteus.

## Running

```bash
sbt "examples/runMain proteus.examples.routeguide.RouteGuideExample"
```

## Code Structure

**Messages & Service** (`RouteGuide.scala`):
```scala
case class Point(latitude: Int, longitude: Int) derives Schema
case class Rectangle(lo: Point, hi: Point) derives Schema
case class Feature(name: String, location: Point) derives Schema
case class RouteNote(location: Point, message: String) derives Schema
case class RouteSummary(pointCount: Int, featureCount: Int, distance: Int, elapsedTime: Int) derives Schema

val getFeatureRpc = Rpc.unary[Point, Feature]("GetFeature")
val listFeaturesRpc = Rpc.serverStreaming[Rectangle, Feature]("ListFeatures")
val recordRouteRpc = Rpc.clientStreaming[Point, RouteSummary]("RecordRoute")
val routeChatRpc = Rpc.bidiStreaming[RouteNote, RouteNote]("RouteChat")

val routeGuideService = Service("routeguide", "RouteGuide")
  .rpc(getFeatureRpc)
  .rpc(listFeaturesRpc)
  .rpc(recordRouteRpc)
  .rpc(routeChatRpc)
```

**Server** (`RouteGuideServer.scala`):
```scala
class RouteGuideServer(port: Int, routeNotes: Ref[Map[Point, List[RouteNote]]]) {
  def getFeature(point: Point): UIO[Feature] = // Simple RPC
  def listFeatures(rectangle: Rectangle): ZStream[Any, Nothing, Feature] = // Server streaming  
  def recordRoute(points: ZStream[Any, StatusException, Point]): IO[StatusException, RouteSummary] = // Client streaming
  def routeChat(notes: ZStream[Any, StatusException, RouteNote]): ZStream[Any, StatusException, RouteNote] = // Bidirectional streaming

  val service = ServerService(using ZioServerBackend)
    .rpc(getFeatureRpc, getFeature)
    .rpc(listFeaturesRpc, listFeatures) 
    .rpc(recordRouteRpc, recordRoute)
    .rpc(routeChatRpc, routeChat)
    .build(routeGuideService)

  val server = ServerBuilder.forPort(port).addService(service).build()
}
```

**Client** (`RouteGuideClient.scala`):
```scala
class RouteGuideClient(host: String, port: Int) {
  val channel: ManagedChannel = ManagedChannelBuilder.forAddress(host, port).usePlaintext().build()
  val backend = new ZioClientBackend(ZChannel(channel, Seq.empty))
  
  def getFeature(point: Point): Task[Feature] = // Simple RPC call
  def listFeatures(rectangle: Rectangle): Task[List[Feature]] = // Server streaming call  
  def recordRoute(points: List[Point]): Task[RouteSummary] = // Client streaming call
  def routeChat(): Task[List[RouteNote]] = // Bidirectional streaming call

  val runDemo: Task[Unit] = // ZIO-based demo runner
}
```

## Generated Protobuf

```protobuf
syntax = "proto3";

package routeguide;

service RouteGuide {
    rpc GetFeature (Point) returns (Feature) {}
    rpc ListFeatures (Rectangle) returns (stream Feature) {}
    rpc RecordRoute (stream Point) returns (RouteSummary) {}
    rpc RouteChat (stream RouteNote) returns (stream RouteNote) {}
}

message Point {
    int32 latitude = 1;
    int32 longitude = 2;
}

message Feature {
    string name = 1;
    Point location = 2;
}

message Rectangle {
    Point lo = 1;
    Point hi = 2;
}

message RouteSummary {
    int32 point_count = 1;
    int32 feature_count = 2;
    int32 distance = 3;
    int32 elapsed_time = 4;
}

message RouteNote {
    Point location = 1;
    string message = 2;
}
```

