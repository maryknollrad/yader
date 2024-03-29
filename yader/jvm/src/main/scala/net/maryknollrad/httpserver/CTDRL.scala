package net.maryknollrad.httpserver

import scalatags.Text.all.*
import net.maryknollrad.ctdose.DB
import net.maryknollrad.yader.Constants.*
import net.maryknollrad.ctdose.DB.QueryInterval
import cats.effect.IO

object CTDRL:
    def selects(targetFunction: String, sId: String, ss: List[String], sValue: String, bigFont: Boolean = false) = 
        val sclass = "downdown-content z-[1] select w-full max-w-xs " ++ (if bigFont then "text-2xl" else "text")
        select(id := sId, 
            onchange := s"JS.${targetFunction}('$sId');", cls := sclass, 
            ss.map(o => 
                if (o == sValue) then option(selected, o)
                else option(o)))

    private def catSelects(targetFName: String, sId: String, ss: List[String], sValue: String) = 
        div(id := "category", cls := "flex flex-row items-center",
            h3(cls := "text-2xl mr-20", "Scheme"),
            selects(targetFName, sId, ss, sValue, true))

    def editCT(db: DB, catIndex: Int = 0) = 
        import cats.effect.IO
        for 
            cats    <-      db.getCategories()
            catName =       cats(catIndex)._2
            drls    <-      db.getLabels(catName)
            ss      <-      db.getStudies(catName)
            nonrs   <-      db.getNonMatchedStudies(catName)
            _       <-      if nonrs.nonEmpty then {
                                db.matchToNone(catName, nonrs) *> 
                                db.log(s"Matched ${nonrs.length} studies to NONE at DRL scheme from '${catName.toUpperCase()}'", DB.LogType.Info)
                            } else 
                                IO(0)
        yield 
            def tabulate(t: String, ss: List[(String, Int, String)]) = 
                div(div(cls := "text-2xl", t),
                    table(cls := "table table-zebra table-auto grow-0",
                        thead(tr(Seq("Study Description", "Label").map(th(_)))),
                        tbody(ss.map((sname, sid, label) => 
                            tr(td(sname), 
                            td(selects("editDRLChanged", s"drl_${cats(catIndex)._1}_$sid", drls, label)))))))
            div(id := "editct", cls := "flex flex-col gap-3 place-items-center",
                catSelects("editDRLChanged", "catSelect", cats.map(_._2), catName),
                div(id := "tableContainer", 
                    if nonrs.nonEmpty then tabulate("Non matched studies", nonrs)
                    else { Seq.empty[Modifier] },
                    tabulate("Matched studies", ss)
            )).toString

    def showStat(db: DB, category: Option[String]) = 
        db.getCategories().map(cats =>
            val catName = category.flatMap(c => cats.find(_._2.toLowerCase() == c.toLowerCase()).map(_._2)).getOrElse(cats.head._2)
            div(id := "drlstats", cls := "flex flex-col gap-3 place-items-center",
                catSelects("updateDrlSummary", drlSummaryCategoryId, cats.map(_._2), catName),
                Contents.intervalButtons(),
                div(id := drlResultId, 
                    data.hx.get := s"/c/drlsummary/$catName/0", data.hx.target := s"#$drlResultId", data.hx.trigger := "load")
            ).toString)

    private def showDrlStats(s: DrlSummary) = 
        table(cls := "table", 
            thead(tr(th("Label"), th("Time"), th("less than DRL"), th("Trend"), th("Count"))),
            tbody(
                s.map({ case (label, time, count, total, ratio, trend) =>
                    tr(td(label), td(time), td(f"$ratio%.1f%%"), td(f"$trend%+.1f%%"), td(s"$count/$total"))
                    })))

    private def showBpartCoverage(s: DrlSummary) = 
        table(cls := "table", 
            thead(tr(th("Bodypart"), th("Time"), th("Coverage"), th("Count"))),
            tbody(
                s.map({ case (label, time, count, total, ratio, _) =>
                    tr(td(label), td(time), td(f"$ratio%.1f%%"), td(s"$count/$total"))
                    })))

    def drlSummary(db: DB, interval: QueryInterval, category: String, isDLP: Boolean, showNone: Boolean) = 
        val (from, to, adjust) = if interval == QueryInterval.Day then (2, 1, 1) else (1, 0, 0)
        for 
            todaysegstr    <-      db.todaySubTime(interval)
            todaysegment   =       todaysegstr.toInt - adjust
            drlRatio       <-      db.drlSummary(category, interval, from, to, isDLP)
            drlFiltered    =       if showNone then drlRatio else drlRatio.filterNot(_._1 == "NONE") 
            bpartCover     <-      db.bodypartCoverage(category, interval, from, to)
        yield
            div(cls := "flex flex-col gap-3",
                div(cls := "text-2xl", "Exams under DRL"),
                // div(drlRatio.mkString("<BR />")),
                // div("-" * 30),
                showDrlStats(summarize(drlFiltered, todaysegment)),
                div(cls := "text-2xl mt-2", "Bodypart Coverage"),
                // div(bpartCover.mkString("<BR />")),
                // div("-" * 30),
                showBpartCoverage(summarize(bpartCover, todaysegment)),
                button(cls := "text-2xl btn grow-0 justify-self-end", onclick := "JS.getcsv();", "Download CSV")
            ).toString

    type DrlResult = List[(String, String, Boolean, Int)]
    type DrlSummary = List[(String, Int, Int, Int, Double, Double)]

    // assumes two datasets with different times
    // input : (label, timespan (as string), flag, count) - label & timespan must be grouped and ordered
    // timeFilter : filter for time value (same unit as timespan), otherwise past label with one row is included in result
    // output : (label, timespan (as integer), count, total, ratio, trendratio)
    def summarize(r: DrlResult, timeFilter: Int): DrlSummary = 
        def h(r: DrlResult, p: Option[(String, Int, Int, Int, Double)], acc: DrlSummary): DrlSummary = 
            r match
                case Nil => 
                    p match 
                        case Some((plbl, pts, pcount, ptotal, pratio)) => 
                            (plbl, pts, pcount, ptotal, pratio, pratio) :: acc
                        case None => 
                            acc
                case (lbl1, ts1, flag1, count1) :: (lbl2, ts2, flag2, count2) :: tail if lbl1 == lbl2 && ts1 == ts2 =>  // true && false
                    val (tcount, fcount) = if flag1 then (count1, count2) else (count2, count1)
                    val total = tcount + fcount
                    val ratio = ((tcount.toDouble / total) * 1000).round / 10.0
                    p match 
                        case Some((plbl, pts, pcount, ptotal, pratio)) if plbl == lbl1 =>      // previous data => different time
                            h(tail, None, (lbl1, ts1.toInt, tcount, total, ratio, ratio - pratio) :: acc)
                        case Some((plbl, pts, pcount, ptotal, pratio)) =>                      // previous data => different label
                            h(tail, Some(lbl1, ts1.toInt, tcount, total, ratio), (plbl, pts, pcount, ptotal, pratio, pratio) :: acc)
                        case None =>
                            h(tail, Some(lbl1, ts1.toInt, tcount, total, ratio), acc)
                case (lbl, ts, flag, count) :: tail =>
                    val (ratio, adjCount, total) = if flag then (100.0, count, count) else (0.0, 0, count)
                    val tsi = ts.toInt
                    p match 
                        case Some((plbl, pts, pcount, ptotal, pratio)) if plbl == lbl =>      // different time
                            val rtrend = if pts < tsi then ratio - pratio else pratio - ratio // check which time is earlier
                            h(tail, None, (lbl, tsi, adjCount, total, ratio, rtrend) :: acc)
                        case Some((plbl, pts, pcount, ptotal, pratio)) =>                     // different label
                            h(tail, Some(lbl, tsi, adjCount, total, ratio), (plbl, pts, pcount, ptotal, pratio, pratio) :: acc)
                        case None =>
                            h(tail, Some(lbl, tsi, adjCount, total, ratio), acc)
        h(r, None, List.empty)
            .filter(t6 => t6._2 == timeFilter)
            .reverse
