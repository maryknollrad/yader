package net.maryknollrad.httpserver

import cats.effect.*
import org.http4s.* 
import org.http4s.dsl.io.*
import scalatags.Text.all.*
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import net.maryknollrad.ctdose.SQLite
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
        body( // data.theme := "cupcake",
            div(id := "page", cls := "flex flex-col p-5 space-y-2 min-h-screen",
                // div(id := "header", cls := "flex flex-row p-5 content-center rounded-md bg-slate-900 text-emerald-400", 
                div(id := "header", cls := "flex flex-row p-5 content-center rounded-md text-primary-content border-primary bg-primary", 
                    div(cls := "w-1/4 text-5xl align-middle", replace("/c/date"), "Date"),
                    div(cls := "grow text-xl text-right", 
                        div(replace("/c/statsummary"), "Statistics"),
                        div("Maryknoll Hospital"))),
                div(replace("/c/notifications"), "Notifications"),
                div(replace("/c/graphs"), "Graphs"),
                div(id := "modalMark")
                // modal()
            )
        )
    )

    private def notifications(): IO[String] = 
        SQLite.getLastLogs().map(ls =>
            if ls.isEmpty then ""
            else
                // div(id := "noti", cls := "relative p-5 rounded-md bg-slate-900 text-emerald-400",
                div(id := "noti", cls := "relative p-5 rounded-md bg-secondary text-secondary-content",
                    // div(id := "notiLabel", cls := "absolute top-0 left-0 bg-amber-200 text-slate-950 text-xl rounded-sm p-1.5",
                    div(id := "notiLabel", cls := "absolute top-0 left-0 bg-accent text-accent-content border-accent text-xl rounded-sm p-1.5",
                        "Notifications"),
                    div(id := "notiContents", cls := "pt-6 h-[100px] overflow-y-auto text-lg",
                        ls.map(l => div(l))
                    )).toString
        )

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
        div(id := "graphs", cls := "flex flex-col items-center rounded-md bg-primary p-5",
            intervalButtons(),
            div(id := "grdose", cls := "w-1/2"),
            div(id := "grbparts", cls := "w-1/2"),
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
        case GET -> Root / "date" =>
            Ok(LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE))

        case GET -> Root / "statsummary" =>
            Ok(SQLite.getCountAndDoseSum().map((count, dosesum, sdate) => 
                    s"Total $count CT exams, ${dosesum.round} mGy.cm since ${sdate}"))

        case GET -> Root / "notifications" =>
            Ok(notifications())

        case GET -> Root / "graphs" =>
            Ok(graphs())

        case GET -> Root / "modal" / partition / partitionValue / IntVar(i) 
                        if i >= 0 && i <= QueryInterval.qiSize =>
            Api.pAndI(partition, intervals(i).toLowerCase())((pt, it) =>
                val (from, to) = QueryInterval.defaultRange(it)
                SQLite.partitionedQuery(pt, it, Some(partitionValue), from, to).flatMap(ps =>
                    val (p5, outliers) = Data.toBoxData(ps)
                    Ok(modal(p5AndOutliers(partitionValue, p5, outliers)).toString)
                )
            ).getOrElse(NotFound())

        case GET -> Root / "modal" / "trends" / partition / partitionValue / IntVar(i) 
                        if i >= 0 && i <= QueryInterval.qiSize =>
            Ok(modal(trendBoxes(partition, partitionValue, intervals(i).toLowerCase())).toString)
    }