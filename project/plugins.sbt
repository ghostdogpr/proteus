addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.4.8")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.6.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.21.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("com.thesamet"       % "sbt-protoc"               % "1.0.8")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.11.2")
addSbtPlugin("com.github.sbt"     % "sbt-native-packager"      % "1.10.4")
addSbtPlugin("org.scalameta"      % "sbt-native-image"         % "0.4.0")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.20"
