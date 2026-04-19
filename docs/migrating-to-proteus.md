# Migrating to Proteus

In this guide, we'll look at how to migrate an existing schema from ScalaPB and Chimney to Proteus while maintaining backward compatibility.

## Existing schema and code

Let's assume we have the following `.proto` file, which we use with ScalaPB:

```proto
syntax = "proto3";

package proto;

import "scalapb/scalapb.proto";

option (scalapb.options) = {
    preserve_unknown_fields: false
};

message Movie {
    reserved 2;

    int32 id = 1;
    repeated Genre genres = 3;
    string title = 4;
    int32 duration = 5;
    ReleaseStatus released_status = 6;
}

enum Genre {
    GENRE_COMEDY = 0;
    GENRE_DRAMA = 1;
    GENRE_HORROR = 2;
}

message ReleaseStatus {
    message Released {
        int64 release_date = 1;
    }

    message Unreleased {
        optional int64 planned_release_date = 1;
    }

    oneof value {
        Released released = 1;
        Unreleased unreleased = 2;
    }
}
```

Here's the code for our domain entities:
```scala
import java.time.{Duration, OffsetDateTime}

case class Movie(
  id: Int,
  title: String,
  genres: List[Genre],
  duration: Duration,
  releasedStatus: ReleaseStatus
)

enum Genre {
  case Comedy, Drama, Horror
}

enum ReleaseStatus {
  case Released(releaseDate: OffsetDateTime)
  case Unreleased(plannedReleaseDate: Option[OffsetDateTime])
}
```
We use Chimney to transform between our domain types and the generated Protobuf types. The transformation code is somewhat verbose, so it's not shown here, but you can find it in the [examples](https://github.com/ghostdogpr/proteus/blob/main/examples/src/main/scala/proteus/examples/migration/ChimneyTransformations.scala).

Let's look at a specific transformation for the `ReleaseStatus` type:
```scala
given PartialTransformer[proto.migration.ReleaseStatus, ReleaseStatus] =
  PartialTransformer(
    _.value
      .intoPartial[ReleaseStatus]
      .withSealedSubtypeHandledPartial[
        proto.migration.ReleaseStatus.Value.Empty.type
      ](_ => Result.fromValue(ReleaseStatus.Unreleased(None)))
      .transform
  )
```
Instead of failing when the value is `Empty` (which means it's missing from the message, perhaps because it was added later), we convert it into `Unreleased(None)`.  
We will need to maintain this same behavior when migrating to Proteus.

## Schema generation with Proteus

Let's create a `ProtobufDeriver` and a `ProtobufCodec` for the `Movie` type:

```scala
import proteus.*

given ProtobufDeriver      = ProtobufDeriver
given ProtobufCodec[Movie] = ProtobufCodec.derived[Movie]
```
It compiles, great! Now let's take a look at the generated Protobuf schema.
```scala
println(Dependency("migration", "proto").add[Movie].render(Nil))
```
However, this fails with an exception:
```
java.lang.Exception: Error deriving field releaseDate of type Released
        at (...)
Caused by: java.lang.Exception: Unsupported primitive type: OffsetDateTime
```
This happens because Proteus does not support `OffsetDateTime` out of the box. Indeed, Protobuf has no primitive type for `OffsetDateTime`. We'll need to create a custom codec for it.
```scala
import java.time.*

lazy val timeCodec: ProtobufCodec[OffsetDateTime] =
  ProtobufCodec
    .derived[Long]
    .transform[OffsetDateTime](
      millis => OffsetDateTime.ofInstant(
        Instant.ofEpochMilli(millis), ZoneOffset.UTC
      ),
      _.toInstant().toEpochMilli()
    )
```
This codec encodes the `OffsetDateTime` type in the same way as a `Long` (which will be `int64` in the Protobuf schema). We provide functions to transform between `OffsetDateTime` and `Long`.

While we're at it, let's also create a codec for the `Duration` type based on the `Int` type (which will be `int32` in Protobuf):
```scala
lazy val durationCodec: ProtobufCodec[Duration] =
  ProtobufCodec
    .derived[Int]
    .transform[Duration](Duration.ofMillis, _.toMillis.toInt)
```
Finally, we need to tell our deriver to use these codecs:
```scala
given ProtobufDeriver =
  ProtobufDeriver
    .instance(timeCodec)
    .instance(durationCodec)
```
Let's try rendering the schema again:
```proto
syntax = "proto3";

package proto;

message Movie {
    int32 id = 1;
    string title = 2;
    repeated Genre genres = 3;
    int32 duration = 4;
    ReleaseStatus released_status = 5;
}

enum Genre {
    COMEDY = 0;
    DRAMA = 1;
    HORROR = 2;
}

message ReleaseStatus {
    oneof value {
        Released released = 1;
        Unreleased unreleased = 2;
    }
}

message Released {
    int64 release_date = 1;
}

message Unreleased {
    optional int64 planned_release_date = 1;
}
```
Now there are no more errors, but the schema does not exactly match the original. We need some extra steps to ensure backward compatibility.

::: tip
Finding differences between two protobuf schemas can be tricky. Proteus ships a dedicated CLI for this — see [proteus-diff](/proteus-diff).

AI agents like Claude Code are also quite good at this, and can organize the differences in a human-friendly way.
:::

## Backward compatibility

Let's first list the differences between the original schema and the generated one:
- `Movie` should have a `reserved` field
- The order of fields in `Movie` is different
- `Genre` values are missing a prefix
- `Released` and `Unreleased` should be nested inside `ReleaseStatus`

Only the first two are breaking changes in terms of backward compatibility, but let's fix all of them.

First, let's enable the derivation flags we need:
- `AutoPrefixEnums` to prefix the `Genre` values
- `NestedOneOf` to nest the `Released` and `Unreleased` types inside `ReleaseStatus`

```scala
import proteus.ProtobufDeriver.DerivationFlag.*

given ProtobufDeriver =
  ProtobufDeriver
    .enable(AutoPrefixEnums)
    .enable(NestedOneOf)
    // continued
```
And now we get this as expected:
```proto
enum Genre {
    GENRE_COMEDY = 0;
    GENRE_DRAMA = 1;
    GENRE_HORROR = 2;
}

message ReleaseStatus {
    message Released {
        int64 release_date = 1;
    }
    
    message Unreleased {
        optional int64 planned_release_date = 1;
    }
    
    oneof value {
        Released released = 1;
        Unreleased unreleased = 2;
    }
}
```
Next, let's set up the field indexes inside `Movie`.  
First, we need to reserve index 2:
```scala
import proteus.Modifiers.*
import zio.blocks.schema.*

given Schema[Movie] = Schema.derived[Movie]

given ProtobufDeriver =
  ProtobufDeriver
    .modifier[Movie](reserved(2))
    // continued
```
To use a modifier, we need to provide an instance of `Schema` for the type we're applying the modifier to.

To fix the order of fields, we have three options:
- Reorder the fields in the original case class
- Create a new case class with the correct order of fields, and use `transform` to convert between the types
- Use a modifier to change the order of fields

In this case, since there is only one field to move and we want to avoid touching the original type, we'll use the `reserved` modifier to assign a specific field index:
```scala
given ProtobufDeriver =
  ProtobufDeriver
    .modifier[Movie]("title", reserved(4))
    // continued
```
And now we have the following, as expected:
```proto
message Movie {
    reserved 2;
    
    int32 id = 1;
    repeated Genre genres = 3;
    string title = 4;
    int32 duration = 5;
    ReleaseStatus released_status = 6;
}
```
Are we done? Not quite! We also need to ensure that our custom handling of the `Empty` case in the `ReleaseStatus` type still works.

The `Empty` case in Chimney represents the scenario where the `oneof` field is missing from the message. Unlike other fields, oneofs do not have a default value, so decoding will fail by default.  
Usually failing is the right behavior, but in this case, we want to transform it into `Unreleased(None)` instead. This can be done using the `defaultValue` method on the `Schema` instance:
```scala
given Schema[ReleaseStatus] = 
  Schema.derived[ReleaseStatus].defaultValue(ReleaseStatus.Unreleased(None))
```
This code specifies that when decoding a `ReleaseStatus` type, if the `oneof` field is missing, it should be transformed into `Unreleased(None)`.

With this in place, our behavior is fully backward compatible, and we can migrate to Proteus without breaking anything.

A complete, runnable example is available in the [examples](https://github.com/ghostdogpr/proteus/blob/main/examples/src/main/scala/proteus/examples/migration/Main.scala).
