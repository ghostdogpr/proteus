# Proteus

[![Maven Central](https://img.shields.io/maven-central/v/com.github.ghostdogpr/proteus-core_3)](https://central.sonatype.com/artifact/com.github.ghostdogpr/proteus-core_3)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

**Proteus** is a **[Scala](https://www.scala-lang.org/) open source library** for working with **[Protobuf](https://protobuf.dev/)** and **[gRPC](https://grpc.io/)**.

It is designed to be **code-first**, meaning that it is able to generate Protobuf codecs and .proto files directly from your Scala code.

It also provides a **declarative way to define gRPC services** in Scala, a bit like [tapir](https://tapir.softwaremill.com/en/latest/) does for HTTP services. You can define messages, RPCs, and services in Scala, then generate clients and servers for them, using a variety of backends (direct style, Future, ZIO, fs2, Ox).

It is available for Scala 3.3.x LTS and later versions. The core module is available for both Scala JVM and Scala.js.

Proteus also ships **[proteus-diff](https://ghostdogpr.github.io/proteus/proteus-diff)**, a standalone native CLI for detecting breaking changes between `.proto` schemas, usable with any Protobuf project, no JVM required.

### Consult the [Documentation](https://ghostdogpr.github.io/proteus/) to learn how to use Proteus.
