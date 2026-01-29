package proteus.examples.routeguide

import proteus.*

given ProtobufDeriver = ProtobufDeriver

// Basic types
case class Point(latitude: Int, longitude: Int) derives ProtobufCodec
case class Rectangle(lo: Point, hi: Point) derives ProtobufCodec

case class Feature(name: String, location: Point) derives ProtobufCodec
case class RouteNote(location: Point, message: String) derives ProtobufCodec
case class RouteSummary(pointCount: Int, featureCount: Int, distance: Int, elapsedTime: Int) derives ProtobufCodec

// RPC definitions
val getFeatureRpc   = Rpc.unary[Point, Feature]("GetFeature")
val listFeaturesRpc = Rpc.serverStreaming[Rectangle, Feature]("ListFeatures")
val recordRouteRpc  = Rpc.clientStreaming[Point, RouteSummary]("RecordRoute")
val routeChatRpc    = Rpc.bidiStreaming[RouteNote, RouteNote]("RouteChat")

val routeGuideService = Service("routeguide", "RouteGuide")
  .rpc(getFeatureRpc)
  .rpc(listFeaturesRpc)
  .rpc(recordRouteRpc)
  .rpc(routeChatRpc)
