name := "JpaScala"

description := "Example of using Hiberante + Scala"

version := "0.0.1"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:_", "-encoding", "utf8")

libraryDependencies in ThisBuild ++= Seq(
   "org.hibernate" % "hibernate-core" % "4.3.5.Final",
   "org.hibernate" % "hibernate-entitymanager" % "4.3.5.Final",
   "org.hsqldb" % "hsqldb" % "2.3.2",
   "org.springframework" % "spring-webmvc" % "4.0.5.RELEASE",
   "org.springframework" % "spring-core" % "4.0.5.RELEASE",
   "org.springframework" % "spring-beans" % "4.0.5.RELEASE",
   "org.springframework" % "spring-jdbc" % "4.0.5.RELEASE",
   "org.springframework" % "spring-tx" % "4.0.5.RELEASE",
   "org.springframework" % "spring-orm" % "4.0.5.RELEASE"
)