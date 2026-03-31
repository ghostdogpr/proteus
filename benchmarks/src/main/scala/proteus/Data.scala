package proteus

import java.time.*

import io.bullet.borer.Codec
import io.bullet.borer.derivation.MapBasedCodecs.*
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.partial
import io.scalaland.chimney.protobufs.*
import test.test as scalapb
import upickle.default.*
import zio.blocks.schema.*
import zio.blocks.schema.json.JsonCodecDeriver

object Data {

  type DateTime = OffsetDateTime

  object DateTime {
    def unsafeSystemNow(): DateTime = OffsetDateTime.now()

    val timeZoneId: ZoneId = ZoneId.of("Asia/Seoul")

    val min: DateTime = OffsetDateTime.ofInstant(Instant.EPOCH, timeZoneId)

    def ofEpochMilli(millis: Long, zoneId: ZoneId = timeZoneId): DateTime = {
      val instant = Instant.ofEpochMilli(millis)

      OffsetDateTime.ofInstant(instant, zoneId)
    }
  }

  extension (dateTime: DateTime) {
    def toEpochMilli: Long =
      dateTime.toInstant.toEpochMilli
  }

  enum Enum derives Schema, ReadWriter, Codec {
    case E1, E2, E3
  }

  enum OneOfExample derives Schema, ReadWriter, Codec.All {
    case O1(a: Int)
    case O2(b: String)
  }

  case class A(a: Int, b: String, c: A2, d: List[A2], e: Option[A2], f: Map[Int, A2], g: DateTime, h: Enum, i: OneOfExample)
    derives Schema,
      ReadWriter,
      Codec

  case class A2(a: Boolean, b: List[Int]) derives Schema, ReadWriter, Codec

  val simpleData = A(
    a = 1,
    b = "simple",
    c = A2(true, List(1, 2, 3)),
    d = List(A2(false, List(4, 5))),
    e = None,
    f = Map.empty,
    g = DateTime.unsafeSystemNow(),
    h = Enum.E1,
    i = OneOfExample.O1(1)
  )

  val complexData = A(
    a = 42,
    b = "benchmark test message",
    c = A2(false, List(10, 20, 30)),
    d = List(
      A2(true, List(1, 2)),
      A2(false, List(3, 4)),
      A2(true, List(5, 6))
    ),
    e = Some(A2(true, List(100, 200, 300))),
    f = Map(
      1 -> A2(true, List(1)),
      2 -> A2(false, List(2)),
      3 -> A2(true, List(3))
    ),
    g = DateTime.unsafeSystemNow(),
    h = Enum.E2,
    i = OneOfExample.O1(1)
  )

  val largeData = A(
    a = 12345,
    b = "this is a longer test message for benchmarking serialization performance",
    c = A2(true, (1 to 50).toList),
    d = (1 to 20).map(i => A2(i % 2 == 0, List(i, i + 1, i + 2))).toList,
    e = Some(A2(false, (100 to 200).toList)),
    f = (1 to 30).map(i => i -> A2(i % 3 == 0, List(i * 10, i * 20))).toMap,
    g = DateTime.unsafeSystemNow(),
    h = Enum.E3,
    i = OneOfExample.O1(1)
  )

  case class Time(currentTimeMillis: Long) derives Schema, ReadWriter, Codec

  given ReadWriter[OffsetDateTime] =
    readwriter[Time].bimap[OffsetDateTime](
      dt => Time(dt.toEpochMilli),
      wrapper => {
        val millis = wrapper.currentTimeMillis
        if (millis == 0) DateTime.min else DateTime.ofEpochMilli(millis)
      }
    )

  given Codec[OffsetDateTime] =
    Codec
      .of[Time]
      .bimap(
        dt => Time(dt.toEpochMilli),
        wrapper => {
          val millis = wrapper.currentTimeMillis
          if (millis == 0) DateTime.min else DateTime.ofEpochMilli(millis)
        }
      )

  val deriver = ProtobufDeriver.instance(dateTimeCodec)

  val dateTimeCodec: ProtobufCodec[DateTime] =
    Schema[Time]
      .derive(deriver)
      .transform[DateTime](
        wrapper => {
          val millis = wrapper.currentTimeMillis
          if (millis == 0) DateTime.min else DateTime.ofEpochMilli(millis)
        },
        dt => Time(dt.toEpochMilli)
      )

  val proteusCodec = Schema[A].derive(deriver)

  val jsonCodec = Schema[A].derive(JsonCodecDeriver)

  val zioSchemaCodec = zio.schema.codec.ProtobufCodec.protobufCodec(zio.schema.DeriveSchema.gen[A])

  implicit val dateTimeToTime: Transformer[DateTime, scalapb.Time] =
    dt => scalapb.Time(currentTimeMillis = dt.toEpochMilli)

  implicit val timeToDateTime: Transformer[scalapb.Time, DateTime] =
    time => if (time.currentTimeMillis == 0) DateTime.min else DateTime.ofEpochMilli(time.currentTimeMillis)

  implicit val oneOfTransformer: Transformer[OneOfExample, scalapb.OneOfExample] =
    value => scalapb.OneOfExample(value.transformInto[scalapb.OneOfExample.Value])

  implicit val oneOfReverseTransformer: PartialTransformer[scalapb.OneOfExample, OneOfExample] =
    PartialTransformer(oneof =>
      oneof.value
        .intoPartial[OneOfExample]
        .withSealedSubtypeHandledPartial[scalapb.OneOfExample.Value.Empty.type](_ => partial.Result.fromEmpty)
        .transform
    )

  def domainToScalaPB(domain: A): scalapb.A =
    domain.transformInto[scalapb.A]

  def scalaPBToDomain(scalapbObj: scalapb.A): A =
    scalapbObj.transformIntoPartial[A] match {
      case partial.Result.Value(value)   => value
      case partial.Result.Errors(errors) => throw new RuntimeException(s"Transformation failed: $errors")
    }
}

@main
def runTest = {
  import Data.*
  while (true) {
    val data    = largeData
    val encoded = proteusCodec.encode(data)
    val decoded = proteusCodec.decode(encoded)
    // val encoded2 = domainToScalaPB(data).toByteArray
    // val decoded2 = scalaPBToDomain(scalapb.A.parseFrom(encoded2))
  }
}
