lazy val scodecVersion = "1.1.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.jcenterRepo
  ),
  libraryDependencies ++= Seq(
    "org.scodec" %% "scodec-core" % "1.10.2",
    "org.scodec" %% "scodec-bits" % "1.1.1-SNAPSHOT"
  )
)

lazy val core = (project in file("core")).settings(
  commonSettings,
  moduleName := "scodec-image"
)

lazy val root = (project in file("."))
  .aggregate(core)



