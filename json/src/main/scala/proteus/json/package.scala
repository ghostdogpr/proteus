package proteus
package json

import io.circe.*
import zio.blocks.schema.PrimitiveType
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset

import proteus.ProtobufCodec.*
import proteus.ProtobufCodec.MessageField.*
import proteus.internal.*

/**
  * An object to register custom json encoders for protobuf codecs.
  */
case class Registry(instances: Map[ProtobufCodec[?], Encoder[?]]) {

  /**
    * Gets the json encoder for the given codec.
    */
  def get[A](codec: ProtobufCodec[A]): Encoder[A] =
    instances.getOrElse(codec, null).asInstanceOf[Encoder[A]]

  /**
    * Adds a custom json encoder for the given codec.
    */
  def add[A: ProtobufCodec](encoder: Encoder[A]): Registry =
    copy(instances = instances + (ProtobufCodec[A] -> encoder))
}
object Registry {

  /**
    * An empty registry.
    */
  val empty: Registry = Registry(Map.empty)
}

/**
  * Options that affect the way protobuf codecs are encoded to json.
  *
  * @param formatMapEntriesAsKeyValuePairs by default, map entries are encoded as an array of key-value pairs. If this option is set to true, the map entries will be encoded as an array of objects with a `key` and `value` field.
  */
case class Options(formatMapEntriesAsKeyValuePairs: Boolean = false)

object Options {

  /**
    * The default options.
    */
  val default: Options = Options(formatMapEntriesAsKeyValuePairs = false)
}

/**
  * An implicit encoder for protobuf codecs.
  *
  * @param codec the codec to encode.
  * @param registry a registry with custom json encoders.
  * @param options the options to use for the encoding.
  */
implicit def jsonWriterCodec[A](using codec: ProtobufCodec[A], registry: Registry, options: Options): Encoder[A] =
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
                      case _                        => throw new ProteusException(s"Unsupported primitive type: ${c.primitiveType}")
                    }
                  case c: Enum[_]              => Json.fromString(c.namesByValue(b))
                  case c: Message[_]           =>
                    c.deconstructor.deconstruct(registers, offset, b)
                    val nextOffset = RegisterOffset.add(offset, c.usedRegisters)
                    val builder    = List.newBuilder[(String, Json)]
                    var i          = 0
                    while (i < c.fields.length) {
                      val field: SimpleField[?] | Null = c.fields(i) match {
                        case f: SimpleField[?]   => f
                        case f: OneOfField[b]    =>
                          val v = getFromRegister(registers, offset, f.register).asInstanceOf[b]
                          f.cases(f.discriminator.discriminate(v)) match {
                            case sf: SimpleField[?]  => sf
                            case _: ExcludedField[?] => null
                          }
                        case _: ExcludedField[?] => null
                      }
                      if (field != null) {
                        val f    = field
                        val json = loop(getFromRegister(registers, offset, f.register).asInstanceOf[f.codec.Focus], f.codec, nextOffset)
                        if (json ne Json.Null) builder += f.jsonName -> json
                      }
                      i += 1
                    }
                    Json.fromFields(builder.result())
                  case c: Repeated[c, e]       =>
                    val it      = c.deconstructor.deconstruct[e](b)
                    val builder = Vector.newBuilder[Json]
                    while (it.hasNext) {
                      val v = it.next
                      builder += loop(v, c.element, offset)
                    }
                    Json.fromValues(builder.result())
                  case c: RepeatedMap[m, k, v] =>
                    val it      = c.deconstructor.deconstruct(b.asInstanceOf[m[k, v]])
                    val builder = Vector.newBuilder[Json]
                    while (it.hasNext) {
                      val v     = it.next
                      val key   = loop(c.deconstructor.getKey(v), c.element.fields(0).asInstanceOf[SimpleField[?]].codec, offset)
                      val value = loop(c.deconstructor.getValue(v), c.element.fields(1).asInstanceOf[SimpleField[?]].codec, offset)
                      if (options.formatMapEntriesAsKeyValuePairs) builder += Json.obj("key" -> key, "value" -> value)
                      else builder += Json.obj(key.asString.getOrElse(key.noSpaces) -> value)
                    }
                    Json.fromValues(builder.result())
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
