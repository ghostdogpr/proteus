package proteus

import java.time.*
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import zio.blocks.schema.*

import proteus.ProtobufCodecBenchmark.*

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ProtobufCodecBenchmark {

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

  case class Time(currentTimeMillis: Long) derives Schema

  val dateTimeCodec: ProtobufCodec[DateTime] =
    Schema[Time]
      .derive(ProtobufDeriver)
      .transform[DateTime](
        wrapper => {
          val millis = wrapper.currentTimeMillis
          if (millis == 0) DateTime.min else DateTime.ofEpochMilli(millis)
        },
        dt => Time(dt.toEpochMilli)
      )

  val codec = Schema[A].deriving(ProtobufDeriver).instance(TypeName.offsetDateTime, dateTimeCodec).derive

  val encodedSimple  = codec.encode(simpleData)
  val encodedComplex = codec.encode(complexData)
  val encodedLarge   = codec.encode(largeData)

  @Benchmark
  def roundTripSimpleMessage(bh: Blackhole): Unit = {
    val encoded = codec.encode(simpleData)
    bh.consume(codec.decode(encoded))
  }

  @Benchmark
  def roundTripComplexMessage(bh: Blackhole): Unit = {
    val encoded = codec.encode(complexData)
    bh.consume(codec.decode(encoded))
  }

  @Benchmark
  def roundTripLargeMessage(bh: Blackhole): Unit = {
    val encoded = codec.encode(largeData)
    bh.consume(codec.decode(encoded))
  }
}

object ProtobufCodecBenchmark {

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

  enum Enum derives Schema {
    case E1, E2, E3
  }

  enum OneOfExample derives Schema {
    case O1(a: Int)
    case O2(b: String)
  }

  case class A(a: Int, b: String, c: A2, d: List[A2], e: Option[A2], f: Map[Int, A2], g: DateTime, h: Enum, i: OneOfExample) derives Schema

  case class A2(a: Boolean, b: List[Int]) derives Schema
}
