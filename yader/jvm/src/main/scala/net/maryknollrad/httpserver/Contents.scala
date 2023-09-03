package net.maryknollrad.httpserver

import cats.effect.*
import org.http4s.* 
import org.http4s.dsl.io.*
import scalatags.Text.all.*
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import net.maryknollrad.ctdose.SQLite

object Contents:
    private def replace(targetUrl: String) = Seq[Modifier](data.hx.get := targetUrl, data.hx.trigger := "load")

    val index = "<!DOCTYPE html>" + html(
        data.theme := "light",
        head(
            title := "YADER",
            meta(charset := "UTF-8"),
            meta(name := "viewport", content := "width=device-width, initial-scale=1.0"),
            script(src := "/assets/htmx.min.js"),
            script(src := "/assets/apexcharts.js"), // manually downloaded js and copied to htest/jvm/src/main/resources
            script(src := "yader.js"),
            link(href := "/assets/yader.css", rel := "stylesheet")
        ),
        body(data.theme := "dracula",
            div(id := "page", cls := "flex flex-col p-5 space-y-2 min-h-screen",
                div(id := "header", cls := "flex flex-row p-5 content-center rounded-md bg-slate-900 text-emerald-400", 
                    div(cls := "w-1/4 text-5xl align-middle", replace("/c/date"), "Date"),
                    div(cls := "grow text-xl text-right", 
                        div(replace("/c/statsummary"), "Statistics"),
                        div("Maryknoll Hospital"))),
                div(replace("/c/notifications"), "Notifications"),
                div(replace("/c/graphs"), "Graphs"),
            )
        )
    )

    def notifications(): IO[String] = 
        SQLite.getLastLogs().map(ls =>
            if ls.isEmpty then ""
            else
                div(id := "noti", cls := "relative p-5 rounded-md bg-slate-900 text-emerald-400",
                    div(id := "notiLabel", cls := "absolute top-0 left-0 bg-amber-200 text-slate-950 text-xl rounded-sm p-1.5",
                        "Notifications"),
                    div(id := "notiContents", cls := "pt-6 h-[100px] overflow-y-auto text-lg",
                        ls.map(l => div(l))
                    )).toString
        )

    private def intervalButtons(selected: Int = 0) = 
        div(id := "intervals", cls := "flex flex-row p-4 space-x-12 justify-center items-center",
            div(id := "intLabl", cls := "text-2xl font-bold amber-300", "Query Interval"),
            div(id := "intBtns", cls := "btn-group",
                Seq("Day", "Week", "Month", "Year").
                zipWithIndex.map((int, i) => 
                    val c = "btn" ++ (if i == selected then " btn-active" else "")
                    button(id := s"ibtn$i", cls := c, onclick := s"JS.btnClick($i)", int))
            )
        )

    private def graphs() = 
        div(id := "graphs", cls := "flex flex-col items-center rounded-md bg-slate-900 p-5",
            intervalButtons(),
            div(id := "grdose", cls := "w-1/2"),
            div(id := "grbparts", cls := "w-1/2"),
            script("JS.setupGraphs('grdose', 'grbparts')")
        ).toString

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
    }