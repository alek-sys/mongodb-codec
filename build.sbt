lazy val commonSettings = Defaults.coreDefaultSettings ++ Seq(
  organization := "com.alexnesterov",
  version := "0.1.0",
  scalaVersion := "2.11.7",
  logLevel := Level.Error,

  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases")
)

lazy val macros = project.
  settings(commonSettings).
  settings(
    libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % "1.1.0",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val core = project.
  settings(commonSettings).
  settings(
    libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % "1.1.0"
  ).dependsOn(macros)