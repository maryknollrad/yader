package net.maryknollrad.d4cs

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import org.dcm4che3.data.Attributes

object DicomBase:
    type QueryHandler[A] = (DicomTags, Attributes) => A
    given LocalDate2String:Conversion[LocalDate, String] = (d: LocalDate) => d.format(DateTimeFormatter.BASIC_ISO_DATE)

    def getTag(tag: Int, attr: Attributes, encoding: String = "utf-8") = 
        String(attr.getBytes(tag), encoding)

    def printTags(encoding: String = "utf-8")(tags: DicomTags, attr: Attributes): Unit = 
        inline def print(t: Int) = println(getTag(t, attr, encoding))
        tags.query.keys.foreach(print(_))
        tags.retrieve.foreach(print(_))

trait DicomBase:
    val deviceName: String
    val callingAe: String
    val calledAe: String
    val remoteHost: String
    val remotePort: Int
    val hostName: String

case class DicomTags(query: Map[Int, String], retrieve: Seq[Int]):
    def tags() = query.keys.toSeq ++ retrieve

enum RetrieveLevel(val strRepr: String):
    case PatientLevel extends RetrieveLevel("PATIENT")
    case StudyLevel extends RetrieveLevel("STUDY")
    case PatientStudyOnly extends RetrieveLevel("STUDY")
    case SeriesLevel extends RetrieveLevel("SERIES")
    case ImageLevel extends RetrieveLevel("IMAGE")
