package net.maryknollrad.httpserver

import scalatags.Text.all.*
import net.maryknollrad.ctdose.DB

object CTDRL:
    def dropdown(lbl: String, lid: String, f: Frag) = 
        val fmod = modifier(tabindex := 0)

        div(cls := "dropdown", 
            label(id := lid, tabindex := 0, cls := "btn m-1", lbl),
            f)

    def selects() = 
        /*
        table(tabindex := 0, cls := "dropdown-content z-[1] table table-pin-rows h-12 overflow-y-auto",
            tbody(tr(td("None"))),
            thead(tr(th("HEAD"))),
            tbody(tr(td("HEAD - nonenhance")),
                  tr(td("HEAD - angiography"))),
            thead(tr(th("SPINE"))),
            tbody(tr(td("C-SPINE")),
                  tr(td("T-SPINE")),
                  tr(td("L-SPINE")))
            )
        */
        select(cls := "downdown-content z-[1] select w-full max-w-xs", 
            option("None"),
            option("HEAD - nonenhanced"),
            option("HEAD - angiography"),
            option("SPINE - cervical"),
            option("SPINE - thoracic"),
            option("SPINE - lumbar"))

    def editCT(db: DB) = 
        db.getStudies().map(ss =>
            div(
                h3("LIST OF CT STUDIES"),
                div(id := "editct",
                    table(cls := "table table-zebra",
                        thead(tr(Seq("Study Description", "Bodypart").map(th(_)))),
                        tbody(ss.map((name, bodypart) => 
                            tr(td(name), 
                            // td(bodypart))))
                            td(selects()))))
                ))).toString)