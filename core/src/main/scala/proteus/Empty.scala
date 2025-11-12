package proteus

import zio.blocks.schema.*

/**
  * An empty message that contains no fields.
  * It is used when deriving with the [[ProtobufDeriver.DerivationFlag.OptionalAsOneOf]] flag for representing None values.
  */
case class Empty() derives Schema

object Empty {
  given emptyCodec: ProtobufCodec[Empty] = Schema[Empty].derive(ProtobufDeriver)
}
