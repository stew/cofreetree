name := "cofreetree"

scalaVersion := "2.12.1"

scalacOptions := Seq(
  "-feature",
  "-deprecation",
  "-encoding", "utf8",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds",
  "-target:jvm-1.7",
  "-unchecked",
  "-Xcheckinit",
  "-Xfuture",
  "-Xlint",
  "-Xfatal-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Yno-imports",
  "-Yno-predef")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies ++= Seq(
  "org.typelevel"  %% "dogs-core"  % "0.5.1",
  "org.typelevel"  %% "dogs-tests" % "0.5.1",
  "org.typelevel"  %% "cats-laws"  % "0.8.1",
  "org.scalacheck" %% "scalacheck" % "1.13.2",
  "org.scalatest"  %% "scalatest"  % "3.0.0" % "test",
  "org.typelevel"  %% "discipline" % "0.7.1" % "test"
)

scalacOptions in (Compile, console) ~= {_.filterNot("-Yno-imports" == _)}

scalacOptions in (Test, console) <<= (scalacOptions in (Compile, console))
