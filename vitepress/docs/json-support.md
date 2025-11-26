# JSON support

Proteus is able to generate [Circe](https://github.com/circe/circe) JSON `Encoder` instances from `ProtobufCodec` instances, which can be useful for logging requests and responses in gRPC services.

For that, you need to add the following dependency to your `build.sbt` file: 
```scala
"com.github.ghostdogpr" %% "proteus-json" % "0.1.0"
```

Then, importing `proteus.json.*` will automatically provide an implicit `Encoder` for all the types that have a `ProtobufCodec` instance, as long as you also have implicit `Registry` and `Options` in scope.

`Registry` allows you to register custom encoders for specific types instead of deriving them automatically, and you can use `Registry.empty` to get an empty registry.

`Options` allows configuring the encoding behavior, and you can use `Options.default` to get the default options.

```scala
import io.circe.syntax.*
import zio.blocks.schema.*

import proteus.*
import proteus.json.*

given ProtobufDeriver = ProtobufDeriver
given Registry        = Registry.empty
given Options         = Options.default

case class HelloRequest(name: String) derives Schema, ProtobufCodec
val json = HelloRequest("world").asJson.noSpaces
println(json)
// {"name":"world"}
```