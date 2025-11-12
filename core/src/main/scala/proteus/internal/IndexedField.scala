package proteus
package internal

import proteus.ProtobufCodec.MessageField.SimpleField

/**
  * A wrapper for a field and its index.
  */
final private[proteus] case class IndexedField(field: SimpleField[?], index: Int)
