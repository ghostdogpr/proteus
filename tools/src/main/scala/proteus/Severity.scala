package proteus

/**
  * Severity of a detected change under a given [[CompatMode]].
  *
  *   - `Error`   &ndash; breaking change.
  *   - `Warning` &ndash; potentially breaking change.
  *   - `Info`    &ndash; non-breaking change.
  */
enum Severity(val level: Int) {
  case Error   extends Severity(2)
  case Warning extends Severity(1)
  case Info    extends Severity(0)
}
