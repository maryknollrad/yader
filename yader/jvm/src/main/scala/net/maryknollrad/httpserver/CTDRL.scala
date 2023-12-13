package net.maryknollrad.httpserver

import scalatags.Text.all.*
import net.maryknollrad.ctdose.DB

object CTDRL:
    def selects
    (sId: String, ss: List[String], sValue: String) = 
        select(id := sId, 
            onchange := s"JS.editDRLChanged('$sId');", cls := "downdown-content z-[1] select w-full max-w-xs", 
            ss.map(o => 
                if (o == sValue) then option(selected, o)
                else option(o)))

    def editCT(db: DB, catIndex: Int = 0) = 
        import cats.effect.IO
        for 
            cats    <-      db.getCategories()
            catName =       cats(catIndex)._2
            drls    <-      db.getLabels(catName)
            ss      <-      db.getStudies(catName)
        yield 
            div(id := "editct",
                div(id := "category", cls := "flex flex-row items-center",
                    h3(cls := "text-2xl mr-20", "Categories"),
                    selects("catSelect", cats.map(_._2), catName)),
                table(cls := "table table-zebra",
                    thead(tr(Seq("Study Description", "Label").map(th(_)))),
                    tbody(ss.map((sname, sid, label) => 
                        tr(td(sname), 
                        td(selects(s"drl_${cats(catIndex)._1}_$sid", drls, label)))))
            )).toString

    def showStat(c: Contents) = 
        import net.maryknollrad.ctdose.DB.*
        import QueryInterval.*
        import cats.effect.IO

        for 
            ws  <- c.db.drlSummary(1, Week)
            _   <- IO.println(ws.mkString("\n"))
            _   <- IO.println("-" * 40)
            cs  <- c.db.bodypartCoverage(1, Month)
            _   <- IO.println(cs.mkString("\n"))
            r   <- c.db.drlFull(1, Month)
            // _   <- IO.println(r)
        yield r.mkString("<BR />")