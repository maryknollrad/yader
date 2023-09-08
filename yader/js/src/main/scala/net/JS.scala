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
import org.scalajs.dom.HTMLElement

@JSExportTopLevel("JS")
object JS:
    // name corrections
    import anon.{BarHeightOffset => ApData}       
    import anon.{AnimationEnd => AEvent}
    type ApexData = js.Array[Double] | ApData | Double | Null | (js.Tuple2[Double, (js.Array[Double | Null]) | Double])
    type AData = js.Array[Any]
    def apexData(d: AData): AData = js.Array(js.Dynamic.literal(data = d))

    val htmx = js.Dynamic.global.htmx
    private val xhttp = XMLHttpRequest()
    private def setAjaxHandler[A](f: Event => A = (_ => println(xhttp.responseText))) = 
      xhttp.onreadystatechange = e => 
        if xhttp.status == 200 && xhttp.readyState == 4 then 
          f(e)
    
    @JSExport
    def hello() = 
        println("Hello, world")

    private val doseBoxChartId = "doseBox"
    private val categoriesPieChartId = "categoryPie"

    // displayed x-axis category values
    private var categories: js.Array[String] = _

    @JSExport
    def setupGraphs(doseId: String, pieId: String) = 
        val doseDiv = document.getElementById(doseId)
        val pieDiv = document.getElementById(pieId)

        val doseOpt = ApexOptions()
            .setSeries(js.Array(Data(js.Array())))
            .setChart(ApexChart()
                .setId(doseBoxChartId).setType(apexchartsStrings.boxPlot)
                .setToolbar(AutoSelected().setShow(false))
                .setEvents(AEvent()
                    .setXAxisLabelClick((e, ct, cf) => 
                        val i = cf.asInstanceOf[Dynamic].labelIndex.asInstanceOf[Int]
                        println(s"SELECTED CATEGORY IS ${categories(i)}")
                        dialog(s"trends/bodypart/${categories(i)}/$selectedIndex")
                    ).setMarkerClick((e, context, mobj) =>
                        val i = mobj.asInstanceOf[js.Dynamic].dataPointIndex.asInstanceOf[Int]
                        dialog(s"bodypart/${categories(i)}/$selectedIndex")
                    )))
            .setNoData(ApexNoData().setText("No Data To Display").setStyle(FontFamilyFontSize().setFontSize("20")))
            .setXaxis(ApexXAxis().setType(apexchartsStrings.category))
            .setYaxis(ApexYAxis().setLabels(Align().setFormatter(truncate)))
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
        import scalajs.js.JSConverters.*
        import scala.collection.mutable.{Seq => MSeq}

        val r = JSON.parse(xhttp.responseText)

        categories = r.bodyparts.asInstanceOf[js.Array[String]]

        val updatePieOption = js.Dynamic.literal(series = r.bodypartsCounts, labels = r.bodyparts)
        ApexCharts.exec(categoriesPieChartId, "updateOptions", updatePieOption)

        val cats = r.bodyparts.asInstanceOf[js.Array[String]]
        val boxData: js.Array[ApexData] = cats.map(c => 
            ApData(x = c, y = r.bodypartBox.selectDynamic(c).selectDynamic("box"))).toJSArray
        val outData: js.Array[ApexData] = cats.map(c => 
            ApData(x = c, y = r.bodypartBox.selectDynamic(c).selectDynamic("outliers"))).toJSArray
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

    // current index of selected interval
    private var selectedIndex = 0

    @JSExport
    def btnClick(index: Int) = 
        if index != selectedIndex then
            htmx.addClass(htmx.find(s"#ibtn$index"), "btn-active")
            htmx.removeClass(htmx.find(s"#ibtn$selectedIndex"), "btn-active")
            selectedIndex = index
            updateCharts()

    @JSExport
    // get url's content and paste into #modalMark
    def dialog(suburl: String) = 
        htmx.ajax("GET", s"/c/modal/$suburl", "#modalMark")
        
    @JSExport
    // called from contents (script) called from above dialog function
    def showDialog() = 
        val dialog = htmx.find("#modalDialog")
        dialog.showModal()