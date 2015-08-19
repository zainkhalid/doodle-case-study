resolvers ++= Seq(
  Resolver.bintrayRepo("scala-js", "scala-js-releases")
)

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.4")

addSbtPlugin("com.lihaoyi" % "workbench" % "0.2.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.4")
