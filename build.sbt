scalaVersion := "3.3.0"
organization := "net.maryknollrad"

val catsEffectVersion = "3.5.1"
val dcm4cheVersion = "5.31.0"
val http4sVersion = "0.23.23"

lazy val yader = (project in file("."))
    .settings(
        resolvers += "SciJava" at "https://maven.scijava.org/content/repositories/public/",
        libraryDependencies ++= Seq(
            "org.typelevel" %% "cats-effect" % catsEffectVersion,
            "org.dcm4che" % "dcm4che-core" % dcm4cheVersion,
            "org.dcm4che" % "dcm4che-net" % dcm4cheVersion,
            "org.dcm4che" % "dcm4che-imageio" % dcm4cheVersion,

            "org.ekrich" %% "sconfig" % "1.5.0",
            "net.sourceforge.tess4j" % "tess4j" % "5.8.0",
            "org.slf4j" % "slf4j-simple" % "2.0.7",
            "org.tpolecat" %% "doobie-core" % "1.0.0-RC4",
            "org.xerial" % "sqlite-jdbc" % "3.42.0.0",
            "eu.timepit" %% "fs2-cron-calev" % "0.8.3",

            "org.http4s" %% "http4s-dsl" % http4sVersion,
            "org.http4s" %% "http4s-ember-server" % http4sVersion,
            "org.http4s" %% "http4s-ember-client" % http4sVersion,
            "com.lihaoyi" %% "upickle" % "3.1.2",
        ),
        // Compile / run / fork := true,
        assembly / mainClass := Some("Demo"),
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
