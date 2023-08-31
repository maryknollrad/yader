package net.maryknollrad.ctdose

import java.time.LocalDate

object KorCTCaterogy:
    private val categories: Map[String, Set[String]] = Map(
        "head" -> Set("brain", "orbit", "omu", "face", "mandible", "temporal"),
        "brainAngio" -> Set("willis"),
        "neck" -> Set("neck", "thyroid"),
        "cervical" -> Set("cervical", "c-spine"),
        "thoracic" -> Set("thoracic", "t-spine", "lung", "thorax", "rib"),
        // "chestLowdose" -> Set("chest", "lung"),
        "coronary" -> Set("cardiac", "heart"),
        "coronaryCalium" -> Set("calcium"),
        "abdomenPelvis" -> Set(),
        // "abdomenPelvisNE" -> Set(),  // additional 
        "abdomen" -> Set("abdomen"),
        "lumbar" -> Set("lumbar", "l-spine")
    )

    def apply(studyDescription: String, bodypart: String, studyDate: LocalDate, patientBirthDate: LocalDate): Option[String] = ???
