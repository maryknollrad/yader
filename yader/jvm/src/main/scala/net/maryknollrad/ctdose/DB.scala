package net.maryknollrad.ctdose

import java.time.{LocalDate, LocalTime}

object DB:
    case class Study(accessionNumber: String, patientId: String, studyDate: LocalDate, studyTime: LocalTime, 
        description: String, protocol: String, bodypart: String, manufacturer: String, model: String, 
        station: String, operator: String, dose1: Double, dose2: Double)

    case class Patient(id: String, sex: String, birthday: LocalDate)

    enum QueryInterval(val strValue: String):
        case Day extends QueryInterval("day")
        case Week extends QueryInterval("week")
        case Month extends QueryInterval("month")
        case Year extends QueryInterval("year")
        
    object QueryInterval:
        val qiSize = QueryInterval.values.length
        def defaultRange(qi: QueryInterval) = if qi == QueryInterval.Day then (1, 0) else (0, 0)

    enum QueryPartition(val strValue: String):
        case Bodypart extends QueryPartition("bodypart")
        // case Operator extends QueryPartition("operator")
        // case Physician extends QueryPartition("physician")

    case class Partitioned(part: String, studydate: String, dateNumber: Int, accessionNumber: String, patientId: String, 
        dose1: Double, dose2: Double, rank: Int) derives upickle.default.ReadWriter
