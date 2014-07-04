name := "funstructs"

version := "1.0"

scalaVersion := "2.11.0"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" % "scalacheck_2.11" % "1.11.4" % "test"
)
