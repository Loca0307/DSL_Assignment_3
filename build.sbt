val akkaVersion = "1.2.1"
val AkkaHttpVersion = "1.3.0"
val scala3Version = "3.7.3"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "ch.usi.si.msde.edsl",
    name := "assignment-03-template",
    version := "2025.1",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-feature"),
    fork := true,
    libraryDependencies += "org.apache.pekko" %% "pekko-actor-typed" % akkaVersion,
    libraryDependencies += "org.apache.pekko" %% "pekko-stream" % akkaVersion,
    libraryDependencies += "org.apache.pekko" %% "pekko-http" % AkkaHttpVersion,
    libraryDependencies += "org.apache.pekko" %% "pekko-http-spray-json" % AkkaHttpVersion
  )
