package proteus.examples.migration

import java.time.*

import zio.blocks.schema.*

import proteus.*
import proteus.Modifiers.*
import proteus.ProtobufDeriver.DerivationFlag.*

given Schema[Movie]         = Schema.derived[Movie]
given Schema[ReleaseStatus] = Schema.derived[ReleaseStatus].defaultValue(ReleaseStatus.Unreleased(None))

given ProtobufDeriver =
  ProtobufDeriver
    .enable(AutoPrefixEnums)
    .enable(NestedOneOf)
    .instance(timeCodec)
    .instance(durationCodec)
    .modifier[Movie](reserved(2))
    .modifier[Movie]("title", reserved(4))

lazy val timeCodec: ProtobufCodec[OffsetDateTime] =
  ProtobufCodec
    .derived[Long]
    .transform[OffsetDateTime](
      millis => OffsetDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC),
      _.toInstant().toEpochMilli().toLong
    )

lazy val durationCodec: ProtobufCodec[Duration] =
  ProtobufCodec
    .derived[Int]
    .transform[Duration](Duration.ofMillis, _.toMillis.toInt)

given ProtobufCodec[Movie] = ProtobufCodec.derived[Movie]
