package net.maryknollrad

import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js
import scala.scalajs.js.{JSON, Object, Dynamic}
import js.annotation.* 
import org.scalajs.dom.{Event, InputEvent, Element, HTMLInputElement, XMLHttpRequest}

// ApexCharts facade thanksfully made by scalablytyped thansfully, let's bear some weird names
import typings.apexcharts.* 
import typings.apexcharts.global.ApexCharts
import typings.apexcharts.ApexCharts.ApexOptions
import typings.apexcharts.anon.*

@JSExportTopLevel("JS")
object JS:
    // name corrections
    import anon.{BarHeightOffset => ApData}       
    type ApexData = js.Array[Double] | ApData | Double | Null | (js.Tuple2[Double, (js.Array[Double | Null]) | Double])
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

        val updatePieOption = js.Dynamic.literal(series = r.bodypartsCounts, labels = r.bodyparts)
        ApexCharts.exec(categoriesPieChartId, "updateOptions", updatePieOption)

        val cats = Object.keys(r.bodypartBox.asInstanceOf[Object])
        val boxData: js.Array[ApexData] = cats.map(c =>
            ApData(x = c, y = r.bodypartBox.selectDynamic(c).selectDynamic("box")))
        val outData: js.Array[ApexData] = cats.map(c =>
            ApData(x = c, y = r.bodypartBox.selectDynamic(c).selectDynamic("outliers")))
        val data = js.Array(
            anon.Data(boxData).setType("boxPlot").setName("box"),
            anon.Data(outData).setType("scatter").setName("outliers"))

        ApexCharts.exec(doseBoxChartId, "updateSeries", data)

    @JSExport 
    def updateCharts() = 
        val uriString = s"/api/graphdata/$selectedIndex"
        setAjaxHandler(updateChartsCB)
        xhttp.open("GET", uriString, true)
        xhttp.send()

    private def truncate[A](d: Double, a: A) = String.format("%.1f", d)

    private var selectedIndex = 0
    @JSExport
    def btnClick(index: Int) = 
        if index != selectedIndex then
            htmx.addClass(htmx.find(s"#ibtn$index"), "btn-active")
            htmx.removeClass(htmx.find(s"#ibtn$selectedIndex"), "btn-active")
            selectedIndex = index
            updateCharts()