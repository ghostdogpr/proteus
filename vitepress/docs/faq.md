# FAQ

## Why not using code generation?

Code generation is the most common way to work with Protobuf and gRPC, and it works great in Scala with a library like [ScalaPB](https://scalapb.github.io/).
However, it has some drawbacks that became increasingly painful in the project I am working on, so I started to explore other options.

A little background: my project is very large and contains 100+ services, 1k+ RPCs and 10k+ messages. On top of that, the generated code is not the kind of code you want to use in your domain logic: it contains only primitive types (no newtypes or refined types), collections are not the best choice (`Seq` instead of `List`, `Vector`, etc), `oneof` types have an akward shape in Scala, etc. It is impossible for the code generation to know exactly the perfect type you'd like to have. What many people end up doing is to convert those generated types back and forth into more idiomatic Scala types, using a library like [Chimney](https://github.com/scalalandio/chimney).

That led to a few issues as the project grew:
- **Increased Compile time**: compiling the thousands of generated files takes time and tend to [slow down IDEs](https://github.com/scalameta/metals/issues/7443) as well. Chimney transformations in large quantities also significantly increase the compile time.
- **Boilerplate**: the transformation code, even using Chimney, is a lot of boilerplate. It became fastidious for us to add new types to our schemas. We also have different Protobuf schemas for different purposes (API, internal, etc) and had to transform each of these to the same Scala types.
- **Inconsistency**: while migrating to Proteus, we found out that the way we were writing .proto files was very inconsistent. Developers were using different conventions and styles for naming things, handling optional fields, etc.
