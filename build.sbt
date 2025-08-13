val scala3Version = "3.7.2"

val grpcVersion            = "1.74.0"
val zioVersion             = "2.1.20"
val zioBlocksSchemaVersion = "0.0.0+519-f9a5c6a6-SNAPSHOT"

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
  .aggregate(core, benchmarks)

lazy val core = project
  .in(file("core"))
  .settings(name := "proteus-core")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++=
      Seq(
        "dev.zio" %% "zio-blocks-schema" % zioBlocksSchemaVersion,
        "io.grpc"  % "grpc-protobuf"     % grpcVersion,
        "dev.zio" %% "zio-test"          % zioVersion % Test,
        "dev.zio" %% "zio-test-sbt"      % zioVersion % Test
      )
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(commonSettings)
  .settings(publish / skip := true)
  .dependsOn(core)
  .enablePlugins(JmhPlugin)

lazy val commonSettings = Def.settings(
  scalacOptions ++= Seq(
    "-deprecation",
    "-preview",
    "-Xfatal-warnings",
    "-no-indent",
    "-Wunused:imports,params,privates,implicits,explicits,nowarn",
    "-Wvalue-discard",
    "-Xkind-projector"
  )
)
