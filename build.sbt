ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "0.9.1"
ThisBuild / organization := "net.maryknollrad"

val catsEffectVersion = "3.5.1"
val http4sVersion = "0.23.25"
val doobieVersion = "1.0.0-RC4"

val jvmSettings = Seq(
    resolvers ++= Seq(
        Resolver.githubPackages("maryknollrad"),
        // for download of dcm4che, which is needed for d4cs
        "SciJava" at "https://maven.scijava.org/content/repositories/public/", 
    ),
    libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-effect" % catsEffectVersion,
        "net.maryknollrad" %% "d4cs" % "0.5.8-SNAPSHOT",
        "net.sourceforge.tess4j" % "tess4j" % "5.9.0",
        "com.lihaoyi" %% "os-lib" % "0.9.3",
    ),
    assembly / assemblyMergeStrategy := {
        case PathList("module-info.class") => MergeStrategy.last
        case path if path.endsWith("/module-info.class") => MergeStrategy.last
        case PathList("versionchanges.txt") => MergeStrategy.last
        // case _ => MergeStrategy.discard
        case x =>
            val oldStrategy = (assembly / assemblyMergeStrategy).value
            oldStrategy(x)
    }
)

lazy val yader = crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full)
    .jsSettings(
      libraryDependencies ++= Seq(
        "org.scalablytyped" %%% "apexcharts" % "3.42.0-3a98ba",
      )
    )
    .jvmSettings(
        // Compile / run / fork := true,
        jvmSettings,
        assembly / mainClass := Some("Yader"),
        libraryDependencies ++= Seq(
            // "org.typelevel" %% "cats-effect" % catsEffectVersion,
            // "net.maryknollrad" %% "d4cs" % "0.5.0-SNAPSHOT",
            "org.ekrich" %% "sconfig" % "1.5.1",
            "org.slf4j" % "slf4j-simple" % "2.0.7",
            "org.tpolecat" %% "doobie-core" % doobieVersion,
            "org.tpolecat" %% "doobie-postgres"  % doobieVersion,
            "org.xerial" % "sqlite-jdbc" % "3.44.1.0",
            "eu.timepit" %% "fs2-cron-calev" % "0.8.3",

            "org.http4s" %% "http4s-dsl" % http4sVersion,
            "org.http4s" %% "http4s-ember-server" % http4sVersion,
            "org.http4s" %% "http4s-ember-client" % http4sVersion,
            "com.lihaoyi" %% "scalatags" % "0.12.0",
            "com.lihaoyi" %% "upickle" % "3.1.2",
            // "com.lihaoyi" %% "mainargs" % "0.5.4"
            "org.gnieh" %% "fs2-data-csv" % "1.10.0",
            "org.gnieh" %% "fs2-data-csv-generic" % "1.10.0",
            "org.http4s" %% "http4s-fs2-data-csv" % "0.2.0",
            "org.slf4j" % "slf4j-simple" % "2.0.10"
        ),
    )

lazy val yaderCli = (project in file("cli"))
    .settings(
        jvmSettings,
        assembly / mainClass := Some("YaderCli"),
        libraryDependencies ++= Seq(            
            "org.slf4j" % "slf4j-nop" % "2.0.10",
        )
    )