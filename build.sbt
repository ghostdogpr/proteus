val scala3Version = "3.7.2"

val grpcVersion            = "1.74.0"
val zioBlocksSchemaVersion = "0.0.0+620-fad4b9a5-SNAPSHOT"
val zioTestVersion         = "2.1.20"
val zioGrpcVersion         = "0.6.3"
val fs2GrpcVersion         = "2.8.2"

inThisBuild(
  List(
    scalaVersion := scala3Version,
    organization := "com.github.ghostdogpr",
    homepage     := Some(url("https://github.com/ghostdogpr/proteus")),
    licenses     := List(License.Apache2),
    scmInfo      := Some(ScmInfo(url("https://github.com/ghostdogpr/proteus/"), "scm:git:git@github.com:ghostdogpr/proteus.git")),
    developers   := List(Developer("ghostdogpr", "Pierre Ricadat", "ghostdogpr@gmail.com", url("https://github.com/ghostdogpr"))),
    resolvers += Resolver.sonatypeCentralSnapshots
  )
)

name := "proteus"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

lazy val root = project
  .in(file("."))
  .settings(publish / skip := true)
  .aggregate(core, grpc, zioGrpc, fs2Grpc, benchmarks, examples)

lazy val core = project
  .in(file("core"))
  .settings(name := "proteus-core")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++=
      Seq(
        "dev.zio" %% "zio-blocks-schema" % zioBlocksSchemaVersion,
        "io.grpc"  % "grpc-protobuf"     % grpcVersion,
        "dev.zio" %% "zio-test"          % zioTestVersion % Test,
        "dev.zio" %% "zio-test-sbt"      % zioTestVersion % Test
      )
  )

lazy val grpc = project
  .in(file("grpc"))
  .settings(name := "proteus-grpc")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++=
      Seq(
        "io.grpc" % "grpc-stub"     % grpcVersion,
        "io.grpc" % "grpc-netty"    % grpcVersion % Test,
        "io.grpc" % "grpc-services" % grpcVersion % Test
      )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val zioGrpc = project
  .in(file("grpc-zio"))
  .settings(name := "proteus-grpc-zio")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++=
      Seq(
        "com.thesamet.scalapb.zio-grpc" %% "zio-grpc-core" % zioGrpcVersion
      )
  )
  .dependsOn(grpc % "compile->compile;test->test")

lazy val fs2Grpc = project
  .in(file("grpc-fs2"))
  .settings(name := "proteus-grpc-fs2")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++=
      Seq(
        "org.typelevel" %% "fs2-grpc-runtime" % fs2GrpcVersion
      )
  )
  .dependsOn(grpc % "compile->compile;test->test")

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(commonSettings)
  .settings(publish / skip := true)
  .dependsOn(core)
  .enablePlugins(JmhPlugin)

lazy val generateProtos = taskKey[Unit]("Generate proto files")

lazy val examples = project
  .in(file("examples"))
  .settings(name := "proteus-examples")
  .settings(commonSettings)
  .settings(publish / skip := true)
  .settings(
    libraryDependencies ++=
      Seq(
        "io.grpc" % "grpc-netty"    % grpcVersion,
        "io.grpc" % "grpc-services" % grpcVersion
      ),
    generateProtos := (Compile / runMain).toTask(" proteus.examples.greeter.Protogen").value
  )
  .dependsOn(grpc)

lazy val commonSettings = Def.settings(
  scalacOptions ++= Seq(
    "-deprecation",
    "-preview",
    "-Xfatal-warnings",
    "-no-indent",
    "-Wunused:imports,params,privates,implicits,explicits,nowarn",
    "-Wvalue-discard",
    "-Xkind-projector"
  ),
  Test / fork := true
)
