package net.maryknollrad

import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js
import scala.scalajs.js.{JSON, Object, Dynamic}
import js.annotation.* 
import org.scalajs.dom.{Event, InputEvent, Element, HTMLInputElement, XMLHttpRequest}

// ApexCharts facade thanksfully made by scalablytyped thansfully, let's bear weird names
import typings.apexcharts.* 
import typings.apexcharts.global.ApexCharts
import typings.apexcharts.ApexCharts.ApexOptions
import typings.apexcharts.anon.*

@JSExportTopLevel("JS")
object JS:
    // name corrections
    import anon.{BarHeightOffset => AData}       
    type ApexData = js.Array[Double] | AData | Double | Null | (js.Tuple2[Double, (js.Array[Double | Null]) | Double | Null])
    type AData = js.Array[Any]
    def apexData(d: AData): AData = js.Array(js.Dynamic.literal(data = d))

    val htmx = js.Dynamic.global.htmx
    private val xhttp = XMLHttpRequest()
    private def setAjaxHandler[A](f: Event => A) = 
      xhttp.onreadystatechange = e => 
        if xhttp.status == 200 && xhttp.readyState == 4 then 
          f(e)

    @JSExport
    def hello() = 
        println("Hello, world")

    private val doseBoxChartId = "doseBox"
    private val categoriesPieChartId = "categoryPie"

    @JSExport
    def setupGraphs(doseId: String, pieId: String) = 
        val doseDiv = document.getElementById(doseId)
        val pieDiv = document.getElementById(pieId)

        val doseOpt = ApexOptions()
            .setSeries(js.Array(Data(js.Array())))
            .setChart(ApexChart()
                .setId(doseBoxChartId).setType(apexchartsStrings.boxPlot)
                .setToolbar(AutoSelected().setShow(false))
                /*
                .setEvents(AEvent().setXAxisLabelClick((e, ct, cf) => 
                    println(s"$e : ${Object.keys(ct.asInstanceOf[Object])} : ${Object.keys(cf.asInstanceOf[Object])}")
                    setAjaxHandler(_ => 
                    println(xhttp.responseText) 
                    )
                    val i = cf.asInstanceOf[Dynamic].selectDynamic("labelIndex").asInstanceOf[Int]
                    println(cats(i))
                    xhttp.open("GET", s"http://localhost:7878/boxdetail/bodypart/${cats(i)}/month", true)
                    xhttp.send()
                */
                ) 
            .setNoData(ApexNoData().setText("No Data To Display").setStyle(FontFamilyFontSize().setFontSize("20")))
            .setXaxis(ApexXAxis().setType(apexchartsStrings.category))
            .setTooltip(ApexTooltip().setTheme("dark").setY(ApexTooltipY().setFormatter(truncate)))
        val doseChart = ApexCharts(doseDiv, doseOpt)
        doseChart.render()

        val pieOpt = ApexOptions()
            .setSeries(js.Array(Data(js.Array())))
            .setLabels(js.Array())
            .setNoData(ApexNoData().setText("No Data To Display").setStyle(FontFamilyFontSize().setFontSize("20")))
            .setChart(ApexChart().setId(categoriesPieChartId).setType(apexchartsStrings.pie))
        val pieChart = ApexCharts(pieDiv, pieOpt)
        pieChart.render()

        updateCharts()

    private def updateChartsCB(e:Event) = 
        val r = JSON.parse(xhttp.responseText)

        val updatePieOption = js.Dynamic.literal(series = r.pie.counts, labels = r.pie.categories)
        ApexCharts.exec(categoriesPieChartId, "updateOptions", updatePieOption)

    @JSExport 
    def updateCharts() = 
        val uriString = s"/api/bpartsdonut"
        setAjaxHandler(updateChartsCB)
        xhttp.open("GET", uriString, true)
        xhttp.send()

    private def truncate[A](d: Double, a: A) = String.format("%.1f", d)
    @JSExport
    def setupCharts(scatterId: String, boxId : String) = 
        // println("Let's setup & draw charts")

        import apexchartsStrings.{category, datetime, numeric}
        type XAxisType = category | datetime | numeric
        def baseOption(tpe: String, xtype: XAxisType) = ApexOptions()
            .setSeries(js.Array(Data(js.Array()).setType(tpe).setName("randoms")))
            .setNoData(ApexNoData().setText("No Data To Display").setStyle(FontFamilyFontSize().setFontSize("20")))
            .setXaxis(ApexXAxis().setType(xtype))
            .setYaxis(ApexYAxis().setLabels(Align().setFormatter(truncate)))
            .setGrid(ApexGrid().setXaxis(Lines().setLines(OffsetYShow().setShow(true))).setYaxis(Lines().setLines(OffsetYShow().setShow(true))))
            .setTooltip(ApexTooltip().setTheme("dark").setY(ApexTooltipY().setFormatter(truncate)))

        val scatterOpt = 
            baseOption("scatter", numeric).setChart(ApexChart().setId("scatterChart").setType(apexchartsStrings.scatter).setToolbar(AutoSelected().setShow(false)))
        val boxOpt = 
            baseOption("boxPlot", category).setChart(ApexChart().setId("boxChart").setType(apexchartsStrings.boxPlot).setToolbar(AutoSelected().setShow(false)))

        val scatterDiv = document.getElementById(scatterId)
        val boxDiv = document.getElementById(boxId)
        val scatterChart = ApexCharts(scatterDiv, scatterOpt)
        val boxChart = ApexCharts(boxDiv, boxOpt)
        scatterChart.render()
        boxChart.render()

    private var callCount = 0
    private def updateChartValues(e:Event) = 
        // println("add data to plots")
        val r = JSON.parse(xhttp.responseText)

        // val sData = js.Array(js.Dynamic.literal(data = r.values.asInstanceOf[js.Array[Any]]))
        ApexCharts.exec("scatterChart", "appendData", apexData(r.values.asInstanceOf[AData]))

        callCount += 1
        // val bData = js.Array(js.Dynamic.literal(data = js.Array(js.Dynamic.literal(x = callCount.toString, y = r.boxdata.asInstanceOf[js.Array[Any]]))))
        val bData = apexData(js.Array(js.Dynamic.literal(x = callCount.toString, y = r.boxdata.asInstanceOf[AData])))
        val bchart = ApexCharts.getChartByID("boxChart").toOption
        bchart.foreach(c => 
            c.appendData(bData)
            c.asInstanceOf[js.Dynamic]._windowResize()  // cast to Dynamic to call non-public method
        )

    @JSExport
    def gatherAndCall() = 
        // println("gatherAndCall")
        val vs = Seq("min", "max", "count").map((id: String) => document.getElementById(id).asInstanceOf[HTMLInputElement].value)
        val uriString = s"/randoms?min=${vs(0)}&max=${vs(1)}&count=${vs(2)}"
        setAjaxHandler(updateChartValues)
        xhttp.open("GET", uriString, true)
        xhttp.send()

    @JSExport
    def updateSlider(source: String, target: String) = 
        val s = document.getElementById(source).asInstanceOf[HTMLInputElement]
        val t = document.getElementById(target)
        t.innerText = s.value

    @JSExport
    def validateAndUpdateSlider(min: String, max: String, minChanged: Boolean) = 
        val minInput = document.getElementById(min).asInstanceOf[HTMLInputElement]
        val maxInput = document.getElementById(max).asInstanceOf[HTMLInputElement]
        val minValue = minInput.value.toInt
        val maxValue = maxInput.value.toInt
        if minValue > maxValue then
            if minChanged then
                val maxceil = maxInput.getAttribute("max").toInt
                if minValue <= maxceil then
                    maxInput.value = minValue.toString
                else
                    minInput.value = maxceil.toString
            else
                val minfloor = minInput.getAttribute("min").toInt
                if maxValue >= minfloor then
                    minInput.value = maxValue.toString
                else
                    maxInput.value = minfloor.toString
        updateSlider(min, min ++ "label")
        updateSlider(max, max ++ "label")