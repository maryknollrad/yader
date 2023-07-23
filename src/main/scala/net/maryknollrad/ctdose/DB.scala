package net.maryknollrad.ctdose

import java.time.{LocalDate, LocalTime}

object DB:
    case class Study(accessionNumber: String, patientId: String, studyDate: LocalDate, studyTime: LocalTime, 
        description: String, protocol: String, bodypart: String, manufacturer: String, model: String, 
        station: String, operator: String, dose: Double)

    case class Patient(id: String, sex: Char, birthday: LocalDate)
    