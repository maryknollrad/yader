scalaVersion := "3.3.0"
organization := "net.maryknollrad"

val catsEffectVersion = "3.5.1"
val dcm4cheVersion = "5.30.0"

lazy val d4cs = (project in file("."))
    .settings(
        resolvers += "SciJava" at "https://maven.scijava.org/content/repositories/public/",
        libraryDependencies ++= Seq(
            "org.typelevel" %% "cats-effect" % catsEffectVersion,
            "org.dcm4che" % "dcm4che-core" % dcm4cheVersion,
            "org.dcm4che" % "dcm4che-net" % dcm4cheVersion,
            "org.dcm4che" % "dcm4che-imageio" % dcm4cheVersion,
            "org.ekrich" %% "sconfig" % "1.5.0",
            "net.sourceforge.tess4j" % "tess4j" % "5.7.0",
            "org.slf4j" % "slf4j-simple" % "2.0.7",
        )
    )