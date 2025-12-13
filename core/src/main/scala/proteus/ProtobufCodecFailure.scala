package proteus

import java.util.ArrayDeque

/**
  * Shared base class for encode/decode failures with an enrichable (mutable) path.
  *
  * Callers are expected to catch a failure, prepend context (message / field), and rethrow the SAME instance.
  */
sealed abstract class ProtobufCodecFailure private[proteus] (val operation: String, cause0: Throwable)
    extends RuntimeException(null, cause0) {
  private val segments = new ArrayDeque[String]()

  private[proteus] final def prepend(segment: String): Unit =
    segments.addFirst(segment)

  private final def pathString(separator: String = " > "): String =
    if (segments.isEmpty) "<root>"
    else {
      val it = segments.iterator()
      val sb = new StringBuilder(it.next())
      while (it.hasNext) {
        sb.append(separator)
        sb.append(it.next())
      }
      sb.toString()
    }

  override final def getMessage: String = {
    val causeMsg = Option(getCause).flatMap(c => Option(c.getMessage)).getOrElse("unknown")
    s"Protobuf $operation failed at ${pathString()}: $causeMsg"
  }
}

/**
  * Decode failure with an enrichable (mutable) path.
  */
final class ProtobufDecodeFailure private (cause0: Throwable) extends ProtobufCodecFailure("decode", cause0)

object ProtobufDecodeFailure {
  def from(cause: Throwable): ProtobufDecodeFailure =
    new ProtobufDecodeFailure(cause)
}

/**
  * Encode failure with an enrichable (mutable) path.
  */
final class ProtobufEncodeFailure private (cause0: Throwable) extends ProtobufCodecFailure("encode", cause0)

object ProtobufEncodeFailure {
  def from(cause: Throwable): ProtobufEncodeFailure =
    new ProtobufEncodeFailure(cause)
}


