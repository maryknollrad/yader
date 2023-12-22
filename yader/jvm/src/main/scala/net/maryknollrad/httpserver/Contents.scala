package net.maryknollrad.httpserver

import cats.effect.*
import org.http4s.* 
import org.http4s.dsl.io.*
import scalatags.Text.all.*
// import scalatags.Text.svgTags.{svg, path}
// import scalatags.Text.svgAttrs.{d, fill, viewBox, stroke, strokeLinecap, strokeLinejoin, strokeWidth}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import net.maryknollrad.ctdose.DB
import net.maryknollrad.ctdose.DB.Partitioned
import org.http4s.dsl.impl.OptionalQueryParamMatcher
import net.maryknollrad.yader.Constants.*

object Contents:
    def replace(targetUrl: String) = Seq[Modifier](data.hx.get := targetUrl, data.hx.trigger := "load")

    val index = "<!DOCTYPE html>" + html(
        data.theme := "cupcake",
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
                div(replace("/c/jobs"), "Jobs"),
                div(id := "contents", div(replace("/c/graphs"), "Graphs")),
                div(id := modalMarkId))
            )
        )

    def intervalButtons(selected: Int = 0) = 
        div(id := "intervals", cls := "flex flex-row p-4 space-x-12 justify-center items-center",
            div(id := "intLabl", cls := "text-2xl font-bold", onclick := "JS.dialog('modal')", "Query Interval"),
            div(id := "intBtns", cls := "join",
                queryIntervals.zipWithIndex.map((interval, i) => 
                    val c = "btn btn-outline btn-ghost" ++ (if i == selected then " btn-active" else "")
                    button(id := s"ibtn$i", cls := c, onclick := s"JS.intBtnClick($i)", interval))
            )
        )

case class Contents(db: DB, institutionName: String, isDLP: Boolean):
    import Contents.intervalButtons

    private def header(dateString: String, count: Int, dosesum: Double, sdate: String, institutionName: String, isDLP: Boolean): String = 
        val doseUnit = if isDLP then "mGy.cm" else "mGy"
        // div(id := "header", cls := "flex flex-row p-5 content-center rounded-md bg-slate-900 text-emerald-400", 
        div(id := "header", cls := "flex flex-row p-5 content-center rounded-md text-primary-content border-primary-content bg-base-300", 
            div(cls := "w-1/4 text-5xl align-middle", dateString),
            div(cls := "grow text-xl text-right mr-2", 
                div(s"Total $count CT exams, ${dosesum.round} ${doseUnit} since ${sdate}"),
                div(institutionName)),
        ).toString

    private def notifications(ls: Seq[(Int, String)]): String = 
        import net.maryknollrad.ctdose.DB.LogType.*
        val classMap = Map(Info.ordinal -> "text-success", Warn.ordinal -> "text-warning", Error.ordinal -> "text-error")
        if ls.isEmpty then ""
        else
            // div(id := "noti", cls := "relative p-5 rounded-md bg-slate-900 text-emerald-400",
            div(id := "noti", cls := "relative p-5 rounded-md border-primary-content bg-base-200 text-secondary-content h-30",
                // div(id := "notiLabel", cls := "absolute top-0 left-0 bg-amber-200 text-slate-950 text-xl rounded-sm p-1.5",
                div(id := "notiLabel", cls := "absolute top-0 left-0 bg-accent text-accent-content border-accent text-xl rounded-sm p-1.5",
                    "Notifications"),
                div(id := "notiContents", cls := "pt-6 h-40 overflow-y-auto text-lg",
                    ls.map((lt, tcontent) => div(cls := classMap.getOrElse(lt, "text-primary-content"), tcontent))
                )).toString

    private def jobs(selected: Int = 0) = 
        div(id := "jobs", cls := "flex flex-row",
            jobTabs.zipWithIndex.map((lbl, i) =>
                val c = "w-1/5 h-10 rounded-t-lg px-4 -mb-2 hover:font-black " ++ (if i == selected then activeTabColor else inactiveTabColor)
                div(id := s"jtab$i", cls := c, onclick := s"JS.tabClick($i);", lbl)
            )).toString

    private def graphs() = 
        div(id := "graphs", cls := "flex flex-col items-center bg-base-100 p-5 space-y-10 rounded-r-md rounded-bl-md",
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
            div(id := trendGraphId),
            script(s"JS.trendGraph('$partition', '$partitionValue', '$interval')")
        )

    import net.maryknollrad.ctdose.DB.QueryInterval
    private object OptionalIntQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Int]("catid")

    val contentService = HttpRoutes.of[IO] {
        case GET -> Root / "header" =>
            val dateString = LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE)
            db.getCountAndDoseSum().flatMap((count, dosesum, sdate) =>
                Ok(header(dateString, count, dosesum, sdate, institutionName, isDLP)))

        case GET -> Root / "notifications" =>
            import net.maryknollrad.ctdose.DB.LogType.*
            db.getLastLogs(ltypes = Seq(Info, Warn, Error)).flatMap(ls => 
                Ok(notifications(ls)))

        case GET -> Root / "jobs" =>
            Ok(jobs())

        case GET -> Root / "tab" / IntVar(i) :? OptionalIntQueryParamMatcher(maybeCat) =>
            i match 
                case 0 =>
                    Ok(graphs())
                case 1 =>
                    Ok(CTDRL.showStat(db))
                case 2 =>
                    Ok(CTDRL.editCT(db, maybeCat.getOrElse(0)))
                case _: Int =>
                    NotFound()

        // graph job related paths
        case GET -> Root / "graphs" =>
            Ok(graphs())

        case GET -> Root / "modal" / partition / partitionValue / IntVar(i) 
                        if i >= 0 && i <= QueryInterval.qiSize =>
            Api.pAndI(partition, queryIntervals(i))((pt, it) =>
                val (from, to) = QueryInterval.defaultRange(it)
                db.partitionedQuery(pt, it, Some(partitionValue), from, to).flatMap(ps =>
                    val (p5, outliers) = Data.toBoxData(ps)
                    Ok(modal(p5AndOutliers(partitionValue, p5, outliers)).toString)
                )
            ).getOrElse(NotFound())

        case GET -> Root / "modal" / "trends" / partition / partitionValue / IntVar(i) 
                        if i >= 0 && i <= QueryInterval.qiSize =>
            Ok(modal(trendBoxes(partition, partitionValue, queryIntervals(i))).toString)

        case GET -> Root / "drlsummary" / category / IntVar(i) if i >= 0 && i <= QueryInterval.qiSize =>
            Ok(CTDRL.drlSummary(db, QueryInterval.fromOrdinal(i), category))

        // DRL edit related paths
    }