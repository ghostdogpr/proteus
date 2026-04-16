package proteus

/**
  * Which compatibility axis to evaluate changes against.
  *
  *   - `Wire`     &ndash; field numbers, wire types, enum value numbers.
  *   - `Source`   &ndash; field names, type names, declaration order.
  *   - `Strictest` &ndash; max(wire, source) severity.
  */
enum CompatMode {
  case Wire, Source, Strictest
}
