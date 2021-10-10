name := "functional-core-imperative-shell-scala"

version := "0.1"

scalaVersion := "2.13.0"

scalacOptions in Test ++= Seq("-Yrangepos")

/////////////////////////////

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.3"

