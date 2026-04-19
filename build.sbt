val scala3Version = "3.3.7"

val grpcVersion                 = "1.80.0"
val scalaProtobufRuntimeVersion = "0.8.16"
val zioBlocksSchemaVersion      = "0.0.33"
val zioTestVersion              = "2.1.25"
val zioGrpcVersion              = "0.6.3"
val fs2GrpcVersion              = "3.0.0"
val oxVersion                   = "1.0.4"
val chimneyVersion              = "1.9.0"
val circeVersion                = "0.14.15"
val zioSchemaVersion            = "1.8.3"
val upickleVersion              = "4.4.3"
val borerVersion                = "1.16.2"
val fastparseVersion            = "3.1.1"
val mainargsVersion             = "0.7.8"

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
  .aggregate(core.jvm, core.js, tools.jvm, tools.js, diff, grpc, zioGrpc, fs2Grpc, oxGrpc, json.jvm, json.js, benchmarks, examples)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(name := "proteus-core")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++=
      Seq(
        "dev.zio" %%% "zio-blocks-schema" % zioBlocksSchemaVersion,
        "dev.zio" %%% "zio-test"          % zioTestVersion % Test,
        "dev.zio" %%% "zio-test-sbt"      % zioTestVersion % Test
      )
  )
  .jvmSettings(
    libraryDependencies ++= Seq("io.grpc" % "grpc-protobuf" % grpcVersion)
  )
  .jsSettings(
    dependencyOverrides += "org.scala-lang" %%% "scala3-library" % scalaVersion.value,
    libraryDependencies ++= Seq("com.thesamet.scalapb" %%% "protobuf-runtime-scala" % scalaProtobufRuntimeVersion),
    Test / fork                              := false
  )

lazy val tools = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("tools"))
  .settings(name := "proteus-tools")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++=
      Seq(
        "com.lihaoyi" %%% "fastparse"    % fastparseVersion,
        "dev.zio"     %%% "zio-test"     % zioTestVersion % Test,
        "dev.zio"     %%% "zio-test-sbt" % zioTestVersion % Test
      )
  )
  .dependsOn(core % "compile->compile;test->test")
  .jsSettings(Test / fork := false)

lazy val diff = project
  .in(file("diff"))
  .settings(name := "proteus-diff")
  .settings(commonSettings)
  .settings(publish / skip := true)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "mainargs"     % mainargsVersion,
      "dev.zio"     %% "zio-test"     % zioTestVersion % Test,
      "dev.zio"     %% "zio-test-sbt" % zioTestVersion % Test
    )
  )
  .enablePlugins(JavaAppPackaging, NativeImagePlugin)
  .settings(
    Compile / mainClass  := Some("proteus.diff.Main"),
    executableScriptName := "proteus-diff",
    nativeImageVersion   := "25.0.1",
    nativeImageJvm       := "graalvm-community",
    nativeImageJvmIndex  := "cs",
    Global / excludeLintKeys ++= Set(nativeImageVersion, nativeImageJvm, nativeImageJvmIndex),
    nativeImageOptions ++= Seq(
      "--no-fallback",
      "--initialize-at-build-time",
      "--gc=epsilon",
      "-O2"
    ),
    Compile / resourceGenerators += Def.task {
      val file = (Compile / resourceManaged).value / "proteus-version.txt"
      IO.write(file, version.value)
      Seq(file)
    }
  )
  .dependsOn(tools.jvm)

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
  .dependsOn(core.jvm % "compile->compile;test->test")

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

lazy val oxGrpc = project
  .in(file("grpc-ox"))
  .settings(name := "proteus-grpc-ox")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++=
      Seq(
        "com.softwaremill.ox" %% "core" % oxVersion
      )
  )
  .dependsOn(grpc % "compile->compile;test->test")

lazy val json = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("json"))
  .settings(name := "proteus-json")
  .settings(commonSettings)
  .settings(libraryDependencies ++= Seq("io.circe" %%% "circe-core" % circeVersion))
  .dependsOn(core % "compile->compile;test->test")
  .jsSettings(Test / fork := false)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(commonSettings)
  .settings(publish / skip := true)
  .settings(
    libraryDependencies ++= Seq(
      "com.thesamet.scalapb" %% "scalapb-runtime"     % scalapb.compiler.Version.scalapbVersion % "protobuf",
      "io.scalaland"         %% "chimney-protobufs"   % chimneyVersion,
      "dev.zio"              %% "zio-schema-protobuf" % zioSchemaVersion,
      "com.lihaoyi"          %% "upickle"             % upickleVersion,
      "io.bullet"            %% "borer-derivation"    % borerVersion
    ),
    Compile / PB.targets := Seq(
      scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
    ),
    scalacOptions ++= Seq("-Wconf:msg=(discarded non-Unit):silent")
  )
  .dependsOn(core.jvm)
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
        "io.grpc"               % "grpc-netty"        % grpcVersion,
        "io.grpc"               % "grpc-services"     % grpcVersion,
        "com.thesamet.scalapb" %% "scalapb-runtime"   % scalapb.compiler.Version.scalapbVersion % "protobuf",
        "io.scalaland"         %% "chimney-protobufs" % chimneyVersion
      ),
    Compile / PB.targets := Seq(
      scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
    ),
    scalacOptions ++= Seq("-Wconf:msg=(discarded non-Unit):silent"),
    generateProtos       := (Compile / runMain).toTask(" proteus.examples.greeter.ProtoGen").value
  )
  .dependsOn(zioGrpc, fs2Grpc, oxGrpc)

lazy val commonSettings = Def.settings(
  scalacOptions ++= Seq(
    "-deprecation",
    "-Xfatal-warnings",
    "-no-indent",
    "-Wunused:imports,params,privates,implicits,explicits,nowarn",
    "-Wvalue-discard",
    "-Ykind-projector"
  ),
  Test / fork := true
)
