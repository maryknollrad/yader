package net.maryknollrad.httpserver

import scalatags.Text.all.*
import net.maryknollrad.ctdose.DB
import net.maryknollrad.yader.Constants.*
import net.maryknollrad.ctdose.DB.QueryInterval
import cats.effect.IO

object CTDRL:
    def selects(targetFunction: String, sId: String, ss: List[String], sValue: String) = 
        select(id := sId, 
            onchange := s"JS.${targetFunction}('$sId');", cls := "downdown-content z-[1] select w-full max-w-xs", 
            ss.map(o => 
                if (o == sValue) then option(selected, o)
                else option(o)))

    private def catSelects(targetFName: String, sId: String, ss: List[String], sValue: String) = 
        div(id := "category", cls := "flex flex-row items-center",
            h3(cls := "text-2xl mr-20", "Categories"),
            selects(targetFName, sId, ss, sValue))

    def editCT(db: DB, catIndex: Int = 0) = 
        import cats.effect.IO
        for 
            cats    <-      db.getCategories()
            catName =       cats(catIndex)._2
            drls    <-      db.getLabels(catName)
            ss      <-      db.getStudies(catName)
        yield 
            div(id := "editct",
                catSelects("editDRLChanged", "catSelect", cats.map(_._2), catName),
                table(cls := "table table-zebra",
                    thead(tr(Seq("Study Description", "Label").map(th(_)))),
                    tbody(ss.map((sname, sid, label) => 
                        tr(td(sname), 
                        td(selects("editDRLChanged", s"drl_${cats(catIndex)._1}_$sid", drls, label)))))
            )).toString

    def showStat(db: DB, catIndex: Int = 0) = 
        db.getCategories().map(cats =>
            val catName = cats(catIndex)._2
            div(id := "drlstats",
                catSelects("updateDrlSummary", drlSummaryCategoryId, cats.map(_._2), catName),
                Contents.intervalButtons(),
                div(id := drlResultId, 
                    data.hx.get := s"/c/drlsummary/$catName/0", data.hx.target := s"#$drlResultId", data.hx.trigger := "load")
            ).toString)

    private def showDrlStat(t5: Tuple5[String, Int, Int, Double, Double]) = 
        val (label, time, count, ratio, trend) = t5
        div(cls := "flex flex-row", 
            div(cls := "text-4xl", label),
            div(cls := "stats shadow",
                div(cls := "stat",
                    div(cls := "stat-title", "Count"),
                    div(cls := "stat-value", count)),
                div(cls := "stat",
                    div(cls := "stat-title", "Less than DRL"),
                    div(cls := "stat-value", ratio)),
                div(cls := "stat",
                    div(cls := "stat-title", "Trend"),
                    div(cls := "stat-value", trend))
            ))

    private def showDrlStats(s: DrlSummary) = 
        table(cls := "table", 
            thead(tr(th("Label"), th("Time"), th("less than DRL"), th("Trend"), th("Count"))),
            tbody(
                s.map({ case (label, time, count, ratio, trend) =>
                    tr(td(label), td(time), td(f"$ratio%.1f%%"), td(f"$trend%.1f%%"), td(count))
                    })))

    private def showBpartCoverage(s: DrlSummary) = 
        table(cls := "table", 
            thead(tr(th("Bodypart"), th("Time"), th("Coverage"), th("Count"))),
            tbody(
                s.map({ case (label, time, count, ratio, _) =>
                    tr(td(label), td(time), td(f"$ratio%.1f%%"), td(count))
                    })))

    def drlSummary(db: DB, interval: QueryInterval, category: String) = 
        val (from, to) = if interval == QueryInterval.Day then (2, 1) else (1, 0)
        for 
            drlRatio    <-      db.drlSummary(category, interval, from, to)
            bpartCover  <-      db.bodypartCoverage(category, interval, from, to)
        yield
            div(
                div(drlRatio.mkString("<BR />")),
                div("-" * 30),
                // summarize(drlRatio).map(showDrlStat),
                showDrlStats(summarize(drlRatio)),
                div("-" * 80),
                div(bpartCover.mkString("<BR />")),
                div("-" * 30),
                showBpartCoverage(summarize(bpartCover))
            ).toString

    type DrlResult = List[(String, String, Boolean, Int)]
    type DrlSummary = List[(String, Int, Int, Double, Double)]

    // assumes two datasets with different times
    // input : (label, timespan (as string), flag, count) - label & timespan must be grouped and ordered
    // output : (label, timespan (as integer), count, option(count), ratio, trendratio)
    def summarize(r: DrlResult): DrlSummary = 
        def h(r: DrlResult, p: Option[(String, Int, Int, Double)], acc: DrlSummary): DrlSummary = 
            r match
                case Nil => 
                    p match 
                        case Some((plbl, pts, pcount, pratio)) => 
                            (plbl, pts, pcount, pratio, pratio) :: acc
                        case None => 
                            acc
                case (lbl1, ts1, flag1, count1) :: (lbl2, ts2, flag2, count2) :: tail if lbl1 == lbl2 && ts1 == ts2 => 
                    val (tcount, fcount) = if flag1 then (count1, count2) else (count2, count1)
                    val ratio = ((tcount.toDouble / (tcount + fcount)) * 1000).round / 10.0
                    p match 
                        case Some((plbl, pts, pcount, pratio)) if plbl == lbl1 =>      // different time
                            h(tail, None, (lbl1, ts1.toInt, tcount, ratio, ratio - pratio) :: acc)
                        case Some((plbl, pts, pcount, pratio)) =>                      // different label
                            h(tail, Some(lbl1, ts1.toInt, tcount, ratio), (plbl, pts, pcount, pratio, pratio) :: acc)
                        case None =>
                            h(tail, Some(lbl1, ts1.toInt, tcount, ratio), acc)
                case (lbl, ts, flag, count) :: tail =>
                    val (ratio, adjCount) = if flag then (100.0, count) else (0.0, 0)
                    val tsi = ts.toInt
                    p match 
                        case Some((plbl, pts, pcount, pratio)) if plbl == lbl =>      // different time
                            val rtrend = if pts < tsi then ratio - pratio else pratio - ratio
                            h(tail, None, (lbl, tsi, adjCount, ratio, rtrend) :: acc)
                        case Some((plbl, pts, pcount, pratio)) =>                      // different label
                            h(tail, Some(lbl, tsi, adjCount, ratio), (plbl, pts, pcount, pratio, pratio) :: acc)
                        case None =>
                            h(tail, Some(lbl, tsi, adjCount, ratio), acc)
        h(r, None, List.empty).reverse
        
        /*
        import cats.effect.IO

        for 
            ws  <- db.drlSummary(catIndex, interval)

            _   <- IO.println(ws.mkString("\n"))
            _   <- IO.println("-" * 40)
            cs  <- db.bodypartCoverage(1, Month)
            _   <- IO.println(cs.mkString("\n"))
            r   <- db.drlFull(1, Month)
            // _   <- IO.println(r)
        yield r.mkString("<BR />")
        */