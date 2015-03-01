import AssemblyKeys._

assemblySettings

name := "instructions"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.4"
    
resolvers ++= Seq(
    Resolver.mavenLocal,
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("public"),
    Resolver.typesafeRepo("releases")
)

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "0.11-M0",
  "org.scalanlp" %% "breeze-natives" % "0.11-M0",
  "org.scalanlp" %% "breeze-viz" % "0.11-M0",
  "org.scalanlp" %% "epic" % "0.3",
  "org.scalanlp" %% "epic-parser-en-span" % "2014.9.15",
  "org.scalanlp" %% "epic-pos-en" % "2015.1.25",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"
)

// see https://github.com/typesafehub/scalalogging/issues/23
testOptions in Test += Tests.Setup(classLoader =>
  classLoader
    .loadClass("org.slf4j.LoggerFactory")
    .getMethod("getLogger", classLoader.loadClass("java.lang.String"))
    .invoke(null, "ROOT")
)

//javaOptions := Seq("-Xmx6g", "-Xrunhprof:cpu=samples,depth=12", "-Dlog4j.configurationFile=src/main/resources/log4j.xml")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("org", "w3c", "dom", _) => MergeStrategy.first
    case PathList("javax", "xml", "stream", _ *) => MergeStrategy.first
    case PathList("scala", "xml", _ *) => MergeStrategy.first
    case PathList("org", "cyberneko", "html", _ *) => MergeStrategy.first
    case PathList("org", "bouncycastle", _ *) => MergeStrategy.first
    case PathList("java_cup", _ *) => MergeStrategy.first
    case PathList("com", "typesafe", "scalalogging", _ *) => MergeStrategy.first
    case x => old(x)
  }
}
