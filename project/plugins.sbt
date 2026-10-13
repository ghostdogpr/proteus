addSbtPlugin("pl.project13.scala" % "sbt-jmh"             % "0.4.8")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"        % "2.6.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"         % "1.22.0")
addSbtPlugin("com.thesamet"       % "sbt-protoc"          % "1.1.0-RC2")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"      % "1.12.1")
addSbtPlugin("com.github.sbt"     % "sbt-native-packager" % "1.13.0")
addSbtPlugin("org.scalameta"      % "sbt-native-image"    % "0.5.0")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.21"
