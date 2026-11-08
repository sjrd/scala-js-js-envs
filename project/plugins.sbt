addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.6.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.2.1")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.2")

// to generate a .wasm file from the build
libraryDependencies += "org.scala-js" %% "scalajs-linker" % "1.22.0"
