ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "com.grey"

lazy val root = (project in file("."))
  .settings(
    name := "grey-sdk",
    libraryDependencies ++= Seq(
      // gRPC
      "io.grpc" % "grpc-netty-shaded" % "1.60.0",
      "io.grpc" % "grpc-protobuf" % "1.60.0",
      "io.grpc" % "grpc-stub" % "1.60.0",
      "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
      
      // JSON (for details serialization)
      "io.circe" %% "circe-core" % "0.14.6",
      "io.circe" %% "circe-generic" % "0.14.6",
      "io.circe" %% "circe-parser" % "0.14.6",
      
      // Testing
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
      "org.scalamock" %% "scalamock" % "5.2.0" % Test
    ),
    
    // Compiler options
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings"
    )
  )

// Enable ScalaPB for protobuf generation
Compile / PB.targets := Seq(
  scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
)
