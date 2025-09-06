package proteus.examples.routeguide

object RouteGuideData {
  // Sample feature database (simplified version of the standard Route Guide data)
  val features = List(
    Feature("Patriots Path, Mendham, NJ 07945, USA", Point(407838351, -746143763)),
    Feature("101 New Jersey 10, Whippany, NJ 07981, USA", Point(408122808, -743999179)),
    Feature("U.S. 206, Somerville, NJ 08876, USA", Point(406109563, -741820800)),
    Feature("35 Deer Creek Rd, Flemington, NJ 08822, USA", Point(407318874, -741835735)),
    Feature("6324 8th Ave, Brooklyn, NY 11220, USA", Point(404318328, -740835638)),
    Feature("1 Merck Access Rd, Whitehouse Station, NJ 08889, USA", Point(405314315, -740647667)),
    Feature("78-98 Schalck Rd, Narrowsburg, NY 12764, USA", Point(413628156, -749015468)),
    Feature("367 Prospect Park West, Brooklyn, NY 11215, USA", Point(404521211, -740188806)),
    Feature("10-70 Astoria Blvd, East Elmhurst, NY 11369, USA", Point(407113723, -738632928)),
    Feature("La Guardia Airport (LGA), Flushing, NY 11371, USA", Point(407109817, -738648902))
  )

  // Helper methods for calculations
  def toRadians(degrees: Int): Double = degrees * Math.PI / 180.0 / 1e7

  def calcDistance(start: Point, end: Point): Int = {
    val lat1 = toRadians(start.latitude)
    val lat2 = toRadians(end.latitude)
    val lon1 = toRadians(start.longitude)
    val lon2 = toRadians(end.longitude)
    
    val deltaLat = lat2 - lat1
    val deltaLon = lon2 - lon1
    
    val a = Math.sin(deltaLat / 2) * Math.sin(deltaLat / 2) +
            Math.cos(lat1) * Math.cos(lat2) * 
            Math.sin(deltaLon / 2) * Math.sin(deltaLon / 2)
    
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    val earthRadius = 6371000 // Earth radius in meters
    
    (earthRadius * c).toInt
  }

  def findFeature(point: Point): Feature = {
    features.find(_.location == point).getOrElse(Feature("", point))
  }

  def findFeaturesInRectangle(rect: Rectangle): List[Feature] = {
    val left = Math.min(rect.lo.longitude, rect.hi.longitude)
    val right = Math.max(rect.lo.longitude, rect.hi.longitude)
    val top = Math.max(rect.lo.latitude, rect.hi.latitude)
    val bottom = Math.min(rect.lo.latitude, rect.hi.latitude)

    features.filter { feature =>
      val point = feature.location
      point.longitude >= left && point.longitude <= right &&
      point.latitude >= bottom && point.latitude <= top
    }
  }
}