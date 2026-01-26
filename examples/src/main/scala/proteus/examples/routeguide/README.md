# Route Guide Example

A comprehensive gRPC example demonstrating all four types of service methods using Proteus with both ZIO and FS2 backends.

## Running

**ZIO Version:**
```bash
sbt "examples/runMain proteus.examples.routeguide.zio.RouteGuideExample"
```

**FS2 Version:**
```bash
sbt "examples/runMain proteus.examples.routeguide.fs2.RouteGuideExample"
```

## Code Structure

The example is structured with shared definitions at the root level and backend-specific implementations in subfolders:

```
routeguide/
├── RouteGuide.scala      # Shared message definitions and service
├── RouteGuideData.scala  # Shared sample data and utilities
├── zio/                  # ZIO-based implementation
│   ├── RouteGuideClient.scala
│   ├── RouteGuideServer.scala
│   └── RouteGuideExample.scala
└── fs2/                  # FS2/Cats Effect implementation
    ├── RouteGuideClient.scala
    ├── RouteGuideServer.scala
    └── RouteGuideExample.scala
```

**Messages & Service** (`RouteGuide.scala`):
```scala
case class Point(latitude: Int, longitude: Int) derives ProtobufCodec
case class Rectangle(lo: Point, hi: Point) derives ProtobufCodec
case class Feature(name: String, location: Point) derives ProtobufCodec
case class RouteNote(location: Point, message: String) derives ProtobufCodec
case class RouteSummary(pointCount: Int, featureCount: Int, distance: Int, elapsedTime: Int) derives ProtobufCodec

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
