package net.maryknollrad.d4cs

enum RetrieveLevel(val strRepr: String):
    case PatientLevel extends RetrieveLevel("PATIENT")
    case StudyLevel extends RetrieveLevel("STUDY")
    case PatientStudyOnly extends RetrieveLevel("STUDY")
    case SeriesLevel extends RetrieveLevel("SERIES")
    case ImageLevel extends RetrieveLevel("IMAGE")
