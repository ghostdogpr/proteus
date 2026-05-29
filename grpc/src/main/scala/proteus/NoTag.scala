package proteus

/**
  * Trivial type evidence for backends that need no per-message type information for streaming.
  * A universal instance is always available, so it imposes no requirement on user code.
  */
final class NoTag[A]

object NoTag {
  given universal[A]: NoTag[A] = new NoTag[A]
}
