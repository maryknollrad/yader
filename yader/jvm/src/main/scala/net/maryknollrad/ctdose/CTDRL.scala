package net.maryknollrad.ctdose

import java.io.File
import scala.util.Try
import org.ekrich.config.* 
import scala.jdk.CollectionConverters.*

object CTDRL:
    def apply(fname: String = "ctdrl") =
        val ifile = File(s"$fname.conf")
        if ifile.exists() then 
            Try:
                val c = ConfigFactory.parseFile(ifile)
                c.entrySet.asScala.foldLeft(Map.empty[String, (Double, Double)])((m, e) =>
                    val ks = e.getKey().split('.')
                    if (ks.length != 2) then throw new Exception(s"Wrong key format in ctdrl.info : ${e.getKey()}")
                    val preValues = m.getOrElse(ks(0), (0.0, 0.0))
                    val v = e.getValue().unwrapped match
                        case i: Integer => i.toDouble
                        case d: java.lang.Double => d.toDouble
                        case _ => throw new Exception(s"Unsupported type given in ctdrl.info : ${ks(0)} - ${e.getValue().unwrapped}")
                    ks(1) match
                        case "ctdi" =>
                            m + ((ks(0), (v, preValues._2)))
                        case "dlp" =>
                            m + ((ks(0), (preValues._1, v)))
                        case _ =>
                            throw new Exception(s"Unknown ctdrl.info value : ${ks(1)}")
                )
            .toEither.left.map(_.getMessage())
        else Left(s"Cannot find $fname.conf")
        // logger.info("Processing date : {}", d)