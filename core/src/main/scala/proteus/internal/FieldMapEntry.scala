package proteus
package internal

import proteus.ProtobufCodec.MessageField.SimpleField

final private[proteus] case class FieldMapEntry(field: SimpleField[?], index: Int)
