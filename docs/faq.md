# FAQ

## Why not use code generation?

Code generation is the most common way to work with Protobuf and gRPC, and it works great in Scala with a library like [ScalaPB](https://scalapb.github.io/).
However, it has some drawbacks that became increasingly painful in the project I am working on, so I started to explore other options.

A little background: my project is very large and contains 100+ services, 1k+ RPCs and 10k+ messages. On top of that, the generated code is not the kind of code you want to use in your domain logic: it contains only vanilla Scala types (no newtypes or refined types), collections are not the best choice (`Seq` instead of `List`, `Vector`, etc.), `oneof` types have an awkward shape in Scala, etc. It is impossible for the code generation to know exactly the perfect type you'd like to have. What many people end up doing is converting those generated types back and forth into more idiomatic Scala types, using a library like [Chimney](https://github.com/scalalandio/chimney).

That led to a few issues as the project grew:
- **Increased compile time**: compiling the thousands of generated files takes time and tends to [slow down IDEs](https://github.com/scalameta/metals/issues/7443) as well. Chimney transformations in large quantities also significantly increase the compile time.
- **Boilerplate**: the transformation code, even using Chimney, is a lot of boilerplate. It became tedious for us to add new types to our schemas. We also have different Protobuf schemas for different purposes (API, internal, etc.) and had to transform each of these to the same Scala types.
- **Inconsistency**: while migrating to Proteus, we found out that the way we were writing .proto files was very inconsistent. Developers were using different conventions and styles for naming things, handling optional fields, etc.

## Won't it break backward compatibility whenever I change Scala code?

It is true that changes in Scala code will change the resulting Protobuf schema, potentially breaking backward compatibility.

To deal with this, make sure to always [generate `.proto` files](/proto-file-generation) and commit them to your repository, enforcing that they are always up to date thanks to a CI check. That way, you can detect when your changes break compatibility. When they do, you can then use the [customization features](/customization) to ensure that the parts of the schema that need to stay the same are not affected by the changes.

## How about performance?

It is hard to compete with the speed of code generation, which creates custom encoders and decoders for every single type without the need for generic code. A fairer comparison is to compare Proteus with the combination of code generation and transformations between the generated types and the idiomatic Scala types.

We have a benchmark project that compares the performance of Proteus with ScalaPB and Chimney for various payload sizes. You can find it [here](https://github.com/ghostdogpr/proteus/tree/main/benchmarks). The bottom line is that Proteus is about 1–2x slower (which is still extremely fast), and the difference gets smaller for larger payloads. Proteus allocates quite a lot less memory for all payload sizes.

If you compare it with alternative binary serialization libraries, Proteus is between 1.5 and 2x faster than [upickle](https://github.com/com-lihaoyi/upickle) and [borer](https://github.com/sirthias/borer), with less memory allocations as well.

::: tip
Once we deployed it to production, we found that Proteus was actually a little faster than ScalaPB with Chimney for our use case! So the benchmarks might not be fully realistic. Make sure to measure for your own payloads.
:::
