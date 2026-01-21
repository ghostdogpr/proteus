package proteus

import java.util.concurrent.TimeUnit
import io.bullet.borer.Cbor
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import test.test as scalapb
import upickle.default.*
import Data.*

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class LargeBenchmark {
  @Benchmark
  def proteus(bh: Blackhole): Unit = {
    val encoded = proteusCodec.encode(largeData)
    bh.consume(proteusCodec.decode(encoded))
  }

  @Benchmark
  def scalapb_chimney(bh: Blackhole): Unit = {
    val encoded = domainToScalaPB(largeData).toByteArray
    bh.consume(scalaPBToDomain(scalapb.A.parseFrom(encoded)))
  }

  @Benchmark
  def zio_blocks_json(bh: Blackhole): Unit = {
    val encoded = jsonCodec.encode(largeData)
    bh.consume(jsonCodec.decode(encoded))
  }

  @Benchmark
  def zio_schema_protobuf(bh: Blackhole): Unit = {
    val encoded = zioSchemaCodec.encode(largeData)
    bh.consume(zioSchemaCodec.decode(encoded))
  }

  @Benchmark
  def upickle(bh: Blackhole): Unit = {
    val encoded = writeBinary(largeData)
    bh.consume(readBinary[A](encoded))
  }

  @Benchmark
  def borer(bh: Blackhole): Unit = {
    val encoded = Cbor.encode(largeData).toByteArray
    bh.consume(Cbor.decode(encoded).to[A].value)
  }
}
