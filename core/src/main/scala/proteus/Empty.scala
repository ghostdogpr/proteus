package proteus

import zio.blocks.schema.*

case class Empty() derives Schema

object Empty {
  val emptyCodec = Schema[Empty].derive(ProtobufDeriver)
}
