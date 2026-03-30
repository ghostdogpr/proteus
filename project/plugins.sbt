addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.4.8")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.5.6")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.20.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.1")
addSbtPlugin("com.thesamet"       % "sbt-protoc"               % "1.0.8")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.11.2")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.20"
