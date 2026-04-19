---
layout: home

hero:
  name: "Proteus"
  text: "Protobuf library for Scala"
  tagline: Protobuf goes Scala-first
  image:
    src: /proteus.svg
    alt: Proteus
  actions:
    - theme: brand
      text: Getting Started
      link: /getting-started
    - theme: alt
      text: GitHub
      link: https://github.com/ghostdogpr/proteus

features:
  - title: Code-First
    details: Generate Protobuf codecs directly from your Scala code. No code generation or sbt plugin required.
  - title: Schema Generation
    details: Generate .proto files to document your schema and efficiently track changes. Various customizations are available.
  - title: Declarative gRPC Services
    details: Define gRPC services in Scala. Generate clients and servers from them using direct style, Future, ZIO, fs2 or Ox backends.
  - title: Schema Diffing
    details: Detect breaking changes between .proto schemas with the proteus-diff CLI. Standalone native binary, no JVM required.
    link: /proteus-diff
    linkText: Learn more
---
