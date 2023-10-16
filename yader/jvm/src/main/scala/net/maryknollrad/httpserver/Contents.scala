package net.maryknollrad.httpserver

import cats.effect.*
import org.http4s.* 
import org.http4s.dsl.io.*
import scalatags.Text.all.*
import scalatags.Text.svgTags.{svg, path}
import scalatags.Text.svgAttrs.{d, fill, viewBox, stroke, strokeLinecap, strokeLinejoin, strokeWidth}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import net.maryknollrad.ctdose.DB
import net.maryknollrad.ctdose.DB.Partitioned

object Contents:
    private def replace(targetUrl: String) = Seq[Modifier](data.hx.get := targetUrl, data.hx.trigger := "load")

    val index = "<!DOCTYPE html>" + html(
        data.theme := "luxury",
        head(
            title := "YADER",
            meta(charset := "UTF-8"),
            meta(name := "viewport", content := "width=device-width, initial-scale=1.0"),
            script(src := "/assets/htmx.min.js"),
            script(src := "/assets/apexcharts.js"), // manually downloaded js and copied to htest/jvm/src/main/resources
            script(src := "yader.js"),
            link(href := "/assets/yader.css", rel := "stylesheet")
        ),
        body( 
            div(id := "page", cls := "flex flex-col p-5 space-y-2 min-h-screen",
                div(replace("/c/header"), "Header"),
                div(replace("/c/notifications"), "Notifications"),
                div(id := "tabContent",
                    div(replace("/c/graphs"), "Graphs"),
                    div(id := "modalMark"),
                div(cls := "btm-nav",
                    button(cls := "active",
                        svg(xmlns := "http://www.w3.org/2000/svg", cls := "h-5 w-5", fill := "none",
                                viewBox := "0 0 24 24", stroke := "currentColor",
                                path(strokeLinecap := "round", strokeLinejoin := "round", strokeWidth := "2",
                                        d := "M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"
                                )),
                        span(cls := "btm-nav-label", "Home")),
                    button(
                        svg(xmlns := "http://www.w3.org/2000/svg", cls := "h-5 w-5", fill := "none",
                                viewBox := "0 0 24 24", stroke := "currentColor",
                                path(strokeLinecap := "round", strokeLinejoin := "round", strokeWidth := "2",
                                        d := "M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                                )),
                        span(cls := "btm-nav-label", "Warnings"))))
            )
        ))

case class Contents(db: DB, institutionName: String, isDLP: Boolean):
    private def header(dateString: String, count: Int, dosesum: Double, sdate: String, institutionName: String, isDLP: Boolean): String = 
        val doseUnit = if isDLP then "mGy.cm" else "mGy"
        // div(id := "header", cls := "flex flex-row p-5 content-center rounded-md bg-slate-900 text-emerald-400", 
        div(id := "header", cls := "flex flex-row p-5 content-center rounded-md text-primary-content border-primary bg-primary", 
            div(cls := "w-1/4 text-5xl align-middle", dateString),
            div(cls := "grow text-xl text-right", 
                div(s"Total $count CT exams, ${dosesum.round} ${doseUnit} since ${sdate}"),
                div(institutionName))).toString

    private def notifications(ls: Seq[(Int, String)]): String = 
        import net.maryknollrad.ctdose.DB.LogType.*
        val classMap = Map(Info.ordinal -> "text-success", Warn.ordinal -> "text-warning", Error.ordinal -> "text-error")
        if ls.isEmpty then ""
        else
            // div(id := "noti", cls := "relative p-5 rounded-md bg-slate-900 text-emerald-400",
            div(id := "noti", cls := "relative p-5 rounded-md bg-secondary text-secondary-content h-30",
                // div(id := "notiLabel", cls := "absolute top-0 left-0 bg-amber-200 text-slate-950 text-xl rounded-sm p-1.5",
                div(id := "notiLabel", cls := "absolute top-0 left-0 bg-accent text-accent-content border-accent text-xl rounded-sm p-1.5",
                    "Notifications"),
                div(id := "notiContents", cls := "pt-6 h-40 overflow-y-auto text-lg",
                    ls.map((lt, tcontent) => div(cls := classMap.getOrElse(lt, "text-primary-content"), tcontent))
                )).toString

    private val intervals = Seq("Day", "Week", "Month", "Year")
    private def intervalButtons(selected: Int = 0) = 
        div(id := "intervals", cls := "flex flex-row p-4 space-x-12 justify-center items-center",
            div(id := "intLabl", cls := "text-2xl font-bold", onclick := "JS.dialog('modal')", "Query Interval"),
            div(id := "intBtns", cls := "btn-group",
                intervals.zipWithIndex.map((interval, i) => 
                    val c = "btn" ++ (if i == selected then " btn-active" else "")
                    button(id := s"ibtn$i", cls := c, onclick := s"JS.intBtnClick($i)", interval))
            )
        )

    private def graphs() = 
        // div(id := "graphs", cls := "flex flex-col items-center rounded-md bg-slate-900 p-5",
        div(id := "graphs", cls := "flex flex-col items-center rounded-md bg-primary p-5 space-y-10",
            intervalButtons(),
            div(id := "grdose", cls := "w-4/5"),
            div(id := "grbparts", cls := "w-2/5"),
            script("JS.setupGraphs('grdose', 'grbparts')")
        ).toString

    private def modal(f: Seq[Frag] = Seq.empty) = 
        val dialog = tag("dialog")
        div(id := "dialogDiv",
            dialog(id := "modalDialog", cls := "modal", 
                form(method := "dialog", cls := "modal-box",
                    f,
                    div(cls := "modal-action", 
                        button(cls := "btn", "Close"))
                )
            ),
            script("JS.showDialog()")
        )

    // shows basic stats and outliers within modal window, called when user clicks graph point 
    private def p5AndOutliers(subpartition: String, p5: Seq[Double], outliers: Seq[Partitioned]) = 
        Seq[Frag](
            div(cls := "text-3xl font-semibold", s"Detailed stats - $subpartition"),
            div(cls := "text-xl text-amber-200", "Basic Stats"),
            table(cls := "table",
                thead(th("Min"), th("Q1"), th("Median"), th("Q3"), th("Max")),
                tbody(tr(p5.map(d => td(String.format("%.1f", d)))))),
            div(cls := "text-xl text-amber-200", "Outliers"),
            div(cls := "overflow-y-auto",
                table(cls := "table",
                    thead(th(), th("Date"), th("Accession Number"), th("Patient ID"), th("dose")),
                    tbody(outliers.zipWithIndex.map((p, i) =>
                            tr( th((i+1).toString),
                                td(p.studydate),
                                td(p.accessionNumber),
                                td(p.patientId),
                                td(String.format("%.1f", p.dose1))
                            ))))))

    // shows trend boxgraph within modal window, called when user clicks xaxis point
    private def trendBoxes[A](partition: String, partitionValue: String, interval: String) = 
        Seq[Frag](
            div(cls := "text-3xl font-semibold", s"Trends - $partitionValue"),
            div(id := "trendgraph"),
            script(s"JS.trendGraph('$partition', '$partitionValue', '$interval')")
        )

    import net.maryknollrad.ctdose.DB.QueryInterval

    val contentService = HttpRoutes.of[IO] {
        case GET -> Root / "header" =>
            val dateString = LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE)
            db.getCountAndDoseSum().flatMap((count, dosesum, sdate) =>
                Ok(header(dateString, count, dosesum, sdate, institutionName, isDLP)))

        case GET -> Root / "notifications" =>
            import net.maryknollrad.ctdose.DB.LogType.*
            db.getLastLogs(ltypes = Seq(Info, Warn, Error)).flatMap(ls => 
                Ok(notifications(ls)))

        case GET -> Root / "graphs" =>
            Ok(graphs())

        case GET -> Root / "modal" / partition / partitionValue / IntVar(i) 
                        if i >= 0 && i <= QueryInterval.qiSize =>
            Api.pAndI(partition, intervals(i).toLowerCase())((pt, it) =>
                val (from, to) = QueryInterval.defaultRange(it)
                db.partitionedQuery(pt, it, Some(partitionValue), from, to).flatMap(ps =>
                    val (p5, outliers) = Data.toBoxData(ps)
                    Ok(modal(p5AndOutliers(partitionValue, p5, outliers)).toString)
                )
            ).getOrElse(NotFound())

        case GET -> Root / "modal" / "trends" / partition / partitionValue / IntVar(i) 
                        if i >= 0 && i <= QueryInterval.qiSize =>
            Ok(modal(trendBoxes(partition, partitionValue, intervals(i).toLowerCase())).toString)
    }