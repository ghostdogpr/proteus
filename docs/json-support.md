# JSON support

Proteus is able to generate [Circe](https://github.com/circe/circe) JSON `Encoder` instances from `ProtobufCodec` instances, which can be useful for logging requests and responses in gRPC services.

For that, you need to add the following dependency to your `build.sbt` file:
```scala
"com.github.ghostdogpr" %% "proteus-json" % "0.2.0"
```

Then, importing `proteus.json.*` will automatically provide an implicit `Encoder` for all the types that have a `ProtobufCodec` instance, as long as you also have implicit `Registry` and `Options` in scope.

```scala
import io.circe.syntax.*

import proteus.*
import proteus.json.*

given ProtobufDeriver = ProtobufDeriver
given Registry        = Registry.empty
given Options         = Options.default

case class HelloRequest(name: String) derives ProtobufCodec
val json = HelloRequest("world").asJson.noSpaces
println(json)
// {"name":"world"}
```

::: tip
Field names are automatically converted to camelCase in the JSON output (e.g., `release_date` becomes `releaseDate`). Null values are filtered out.
:::

## Registry

`Registry` allows you to register custom Circe encoders for specific types instead of deriving them automatically. Use `Registry.empty` to get an empty registry.

```scala
import io.circe.*

case class MyId(value: String) derives ProtobufCodec

given Registry = Registry.empty.add[MyId](Encoder[String].contramap(_.value))
```

With this, `MyId` will be encoded as a plain JSON string instead of an object.

## Options

`Options` allows configuring the encoding behavior. Use `Options.default` to get the default options.

The following options are available:
- `formatMapEntriesAsKeyValuePairs`: By default, map entries are encoded as an array of objects where the key is used as the field name (`[{"myKey": "myValue"}]`). When set to `true`, map entries are encoded as an array of objects with explicit `key` and `value` fields (`[{"key": "myKey", "value": "myValue"}]`).

```scala
given Options = Options(formatMapEntriesAsKeyValuePairs = true)
```