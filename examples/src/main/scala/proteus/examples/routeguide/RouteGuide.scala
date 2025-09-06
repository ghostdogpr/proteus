package proteus.examples.routeguide

import zio.blocks.schema.Schema

import proteus.*

// Basic types
case class Point(latitude: Int, longitude: Int) derives Schema
case class Rectangle(lo: Point, hi: Point) derives Schema

case class Feature(name: String, location: Point) derives Schema
case class RouteNote(location: Point, message: String) derives Schema
case class RouteSummary(pointCount: Int, featureCount: Int, distance: Int, elapsedTime: Int) derives Schema

given ProtobufDeriver = ProtobufDeriver

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
