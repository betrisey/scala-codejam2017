name := "ScalaJam"
description := "Scala template for Google Code Jam"
scalaVersion := "2.11.7"
libraryDependencies ++= Seq(
  "com.softwaremill.scalamacrodebug"  %% "macros"       %   "0.4",
  //"org.scalacheck"                    %% "scalacheck"   %   "1.12.4"  % Test,
  "com.github.pathikrit"              %% "better-files" %   "2.15.0",
  "org.scalatest"                     %% "scalatest"    %   "2.2.6"   % Test
)
scalacOptions ++= Seq(  // copied from https://tpolecat.github.io/2014/04/11/scalac-flags.html
  "-deprecation",
  "-encoding", "UTF-8",       // yes, this is 2 args
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDS")
logBuffered in Test := false
parallelExecution in Test := false
