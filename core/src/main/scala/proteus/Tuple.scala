package proteus

// can be replaced with Tuple.Contains once moving to the next Scala LTS
object Tuple {
  type Contains[X <: Tuple, Y] <: Boolean = X match {
    case Y *: _     => true
    case _ *: xs    => Contains[xs, Y]
    case EmptyTuple => false
  }
}
