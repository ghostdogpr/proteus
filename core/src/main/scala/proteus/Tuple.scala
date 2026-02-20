package proteus

// can be replaced with Tuple.Contains once moving to the next Scala LTS
object Tuple {
  type Contains[X <: Tuple, Y] <: Boolean = X match {
    case Y *: _     => true
    case _ *: xs    => Contains[xs, Y]
    case EmptyTuple => false
  }

  type IsElemType[Labels <: Tuple, Types <: Tuple, Name, Expected] <: Boolean = (Labels, Types) match {
    case (Name *: _, Expected *: _) => true
    case (Name *: _, _ *: _)        => false
    case (_ *: ls, _ *: ts)         => IsElemType[ls, ts, Name, Expected]
    case _                          => false
  }
}
