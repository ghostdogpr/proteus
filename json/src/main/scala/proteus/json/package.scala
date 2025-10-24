package proteus
package json

import io.circe.*
import zio.blocks.schema.PrimitiveType
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset

import proteus.ProtobufCodec.*
import proteus.ProtobufCodec.MessageField.*
import proteus.internal.*

case class Registry(instances: Map[ProtobufCodec[?], Encoder[?]]) {
  def get[A](codec: ProtobufCodec[A]): Encoder[A] =
    instances.getOrElse(codec, null).asInstanceOf[Encoder[A]]

  def add[A: ProtobufCodec](encoder: Encoder[A]): Registry =
    copy(instances = instances + (ProtobufCodec[A] -> encoder))
}
object Registry                                                   {
  val empty: Registry = Registry(Map.empty)
}

implicit def jsonWriterCodec[A](using codec: ProtobufCodec[A], registry: Registry): Encoder[A] =
  new Encoder[A] {
    def apply(a: A): Json =
      withRegisters { registers =>
        def loop[B](b: B, codec: ProtobufCodec[B], offset: RegisterOffset): Json =
          if (b == null) Json.Null
          else
            registry.get(codec) match {
              case null    =>
                codec match {
                  case c: Primitive[_]         =>
                    c.primitiveType match {
                      case _: PrimitiveType.Int     => Json.fromInt(b)
                      case _: PrimitiveType.Long    => Json.fromLong(b)
                      case _: PrimitiveType.Boolean => Json.fromBoolean(b)
                      case _: PrimitiveType.String  => Json.fromString(b)
                      case _: PrimitiveType.Double  => Json.fromDoubleOrNull(b)
                      case _: PrimitiveType.Float   => Json.fromFloatOrNull(b)
                      case _                        => throw new Exception(s"Unsupported primitive type: ${c.primitiveType}")
                    }
                  case c: Enum[_]              => Json.fromString(c.namesByValue(b))
                  case c: Message[_]           =>
                    c.deconstructor.deconstruct(registers, offset, b)
                    val nextOffset = RegisterOffset.add(offset, c.usedRegisters)
                    val builder    = List.newBuilder[(String, Json)]
                    var i          = 0
                    while (i < c.fields.length) {
                      val field = c.fields(i) match {
                        case f: SimpleField[?]   => Some(f)
                        case f: OneofField[b]    =>
                          val v = getFromRegister(registers, offset, f.register).asInstanceOf[b]
                          Some(f.cases(f.discriminator.discriminate(v)))
                        case _: ExcludedField[?] => None
                      }
                      field.foreach { f =>
                        builder +=
                          toCamelCase(f.name) ->
                            loop(getFromRegister(registers, offset, f.register).asInstanceOf[f.codec.Focus], f.codec, nextOffset)
                      }
                      i += 1
                    }
                    Json.obj(builder.result()*).dropNullValues
                  case c: Repeated[c, e]       =>
                    val it      = c.deconstructor.deconstruct[e](b)
                    val builder = List.newBuilder[Json]
                    while (it.hasNext) {
                      val v = it.next
                      builder += loop(v, c.element, offset)
                    }
                    Json.arr(builder.result()*)
                  case c: RepeatedMap[m, k, v] =>
                    val it      = c.deconstructor.deconstruct(b.asInstanceOf[m[k, v]])
                    val builder = List.newBuilder[Json]
                    while (it.hasNext) {
                      val v     = it.next
                      val key   = loop(c.deconstructor.getKey(v), c.element.fields(0).asInstanceOf[SimpleField[?]].codec, offset)
                      val value = loop(c.deconstructor.getValue(v), c.element.fields(1).asInstanceOf[SimpleField[?]].codec, offset)
                      builder += Json.obj(key.asString.getOrElse(key.noSpaces) -> value)
                    }
                    Json.arr(builder.result()*)
                  case Bytes                   => Json.fromString("<bytes>")
                  case c: Transform[_, _]      => loop(c.to(b), c.codec, offset)
                  case c: RecursiveMessage[_]  => loop(b, c.codec, offset)
                  case c: Optional[_]          =>
                    b match {
                      case None    => Json.Null
                      case Some(v) => loop(v, c.codec, offset)
                    }
                }
              case encoder => encoder(b)
            }

        loop(a, codec, RegisterOffset.Zero)
      }
  }
