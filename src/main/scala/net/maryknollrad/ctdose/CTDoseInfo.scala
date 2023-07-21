package net.maryknollrad.ctdose

object CTDoseInfo:
    type CTDoseInfo = Map[(String, String), CTDoseSeriesInfo]

    val DoseBy = "dose-series-by"
    val DoseValue = "dose-series-value"
    val ExtReg = "dose-extraction-reg"

    enum CTDoseSeries:
        case SeriesNumber, SeriesIndex, SeriesName

    type CTDoseInfoValue = Int | String

    case class DoseSeriesInfo(by: Option[CTDoseSeries] = None, value: Option[CTDoseInfoValue] = None, regex: Option[String] = None):
        def allNonEmpty() = by.nonEmpty && value.nonEmpty && regex.nonEmpty
        def missingField() = 
            Seq((by, DoseBy), (value, DoseValue), (regex, ExtReg)).foldLeft(Seq.empty[String]):
                case (ans, (f, fs)) => 
                    if f.isEmpty then ans :+ fs
                    else ans
    
    case class CTDoseSeriesInfo(manufacturer: String, model: String, seriesInfo: CTDoseInfo.DoseSeriesInfo)
    case class CTDoseResult()