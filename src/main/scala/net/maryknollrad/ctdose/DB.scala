package net.maryknollrad.ctdose

import java.time.{LocalDate, LocalTime}

object DB:
    case class Study(accessionNumber: String, patientId: String, studyDate: LocalDate, studyTime: LocalTime, 
        description: String, protocol: String, bodypart: String, manufacturer: String, model: String, 
        station: String, operator: String, dose: Double)

    case class Patient(id: String, sex: String, birthday: LocalDate)

    enum QueryInterval:
        case Day, Week, Month
    
    enum QueryPartition:
        case Bodypart

    case class Partitioned(partition: String, dateNumber: Int, dose1: Double, dose2: Double, rank: Int)
    object Partitioned:
        import upickle.default.{ReadWriter => RW, macroRW}
        implicit val rw: RW[Partitioned] = macroRW