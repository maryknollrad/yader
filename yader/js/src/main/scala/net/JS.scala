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
import org.scalajs.dom.HTMLDialogElement
import org.scalajs.dom.HTMLSelectElement
import scala.scalajs.js.URIUtils
import typings.std.stdStrings.window
import net.maryknollrad.yader.Constants.*

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
                        dialog(s"trends/bodypart/${categories(i)}/$intervalIndex")
                    ).setMarkerClick((e, context, mobj) =>
                        val i = mobj.asInstanceOf[js.Dynamic].dataPointIndex.asInstanceOf[Int]
                        dialog(s"bodypart/${categories(i)}/$intervalIndex")
                    )))
            // .setNoData(ApexNoData().setText("No Data To Display").setStyle(FontFamilyFontSize().setFontSize("20")))
            .setNoData(ApexNoData().setText(graphNoDataMsg).setStyle(FontFamilyFontSize().setFontSize("20")))
            .setXaxis(ApexXAxis().setType(apexchartsStrings.category))
            .setYaxis(ApexYAxis().setLabels(Align().setFormatter(truncate)))
            .setTooltip(ApexTooltip().setTheme("dark").setY(ApexTooltipY().setFormatter(truncate)))
        val doseChart = ApexCharts(doseDiv, doseOpt)
        doseChart.render()

        val pieOpt = ApexOptions()
            .setSeries(js.Array(Data(js.Array())))
            .setLabels(js.Array())
            // .setNoData(ApexNoData().setText("No Data To Display").setStyle(FontFamilyFontSize().setFontSize("20")))
            .setNoData(ApexNoData().setText(graphNoDataMsg).setStyle(FontFamilyFontSize().setFontSize("20")))
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

        val dbchart = ApexCharts.getChartByID(doseBoxChartId)
        dbchart.foreach(c =>
            c.updateSeries(data)
            c.asInstanceOf[js.Dynamic]._windowResize()  // force redraw using private method
        )

    @JSExport 
    def updateCharts() = 
        val uriString = s"/api/graphdata/$intervalIndex"
        setAjaxHandler(updateChartsCB)
        xhttp.open("GET", uriString, true)
        xhttp.send()

    private def truncate[A](d: Double, a: A) = String.format("%.1f", d)

    private var tabIndex = 0
    private val activeTab = activeTabColor
    private val inactiveTab = inactiveTabColor

    // current index of selected interval
    // should be stored in browser not server, because multiple user connection needs separate session in server
    private var intervalIndex = 0

    // currently selected category 
    // private var categoryName: String = _

    @JSExport
    def tabClick(index: Int) = 
        if index != tabIndex then
            resetTab()
            htmx.removeClass(htmx.find(s"#jtab$index"), inactiveTab)
            htmx.addClass(htmx.find(s"#jtab$index"), activeTab)
            htmx.removeClass(htmx.find(s"#jtab$tabIndex"), activeTab)
            htmx.addClass(htmx.find(s"#jtab$tabIndex"), inactiveTab)
            tabIndex = index
            htmx.ajax("GET", s"/c/tab/$index", "#contents")

    private def resetTab() = 
        intervalIndex = 0
        tabIndex match 
            case 0 =>   // boxplot
                Seq(doseBoxChartId, categoriesPieChartId).foreach(id => ApexCharts.exec(id, "destroy"))
            case _ =>

    @JSExport
    def intBtnClick(index: Int) = 
        if index != intervalIndex then
            htmx.addClass(htmx.find(s"#ibtn$index"), "btn-active")
            htmx.removeClass(htmx.find(s"#ibtn$intervalIndex"), "btn-active")
            intervalIndex = index
            tabIndex match
                case 0 => updateCharts()
                case 1 => updateDrlSummary()
                case _ => 
            
    @JSExport
    // get url's content and paste into #modalMark
    def dialog(suburl: String) = 
        htmx.ajax("GET", s"/c/modal/$suburl", s"#$modalMarkId")
        
    @JSExport
    // called from contents (script) called from above dialog function
    def showDialog() = 
        val dialog = htmx.find("#modalDialog")
        dialog.showModal()

    private val trendChartId = "trendBox"
    @JSExport
    def trendGraph(partition: String, partitionValue: String, intervalValue: String) = 
        val trendDiv = document.getElementById(trendGraphId)
        val trendOpt = ApexOptions()
            .setSeries(js.Array(Data(js.Array())))
            .setChart(ApexChart()
                .setId(trendChartId).setType(apexchartsStrings.boxPlot)
                .setToolbar(AutoSelected().setShow(false)))
            .setNoData(ApexNoData().setText(graphNoDataMsg).setStyle(FontFamilyFontSize().setFontSize("20")))
            .setXaxis(ApexXAxis().setType(apexchartsStrings.category))
            .setYaxis(ApexYAxis().setLabels(Align().setFormatter(truncate)))
            .setTooltip(ApexTooltip().setTheme("dark").setY(ApexTooltipY().setFormatter(truncate)))
        val doseChart = ApexCharts(trendDiv, trendOpt)
        doseChart.render()

        // destroy graph when modal window closes
        val modalDialog = document.getElementById("modalDialog").asInstanceOf[HTMLDialogElement]
        modalDialog.addEventListener("close", (e: Event) => { doseChart.destroy() })

        setAjaxHandler(trendGraphCB(doseChart))
        xhttp.open("GET", s"/api/trends/$partition/$partitionValue/$intervalValue", true)
        xhttp.send()

    private def trendGraphCB(chart: ApexCharts)(e: Event) = 
        val r = JSON.parse(xhttp.responseText)
        val cats = Object.keys(r.asInstanceOf[Object])
        val boxData: js.Array[ApexData] = cats.map(c => 
            ApData(x = c, y = r.selectDynamic(c).selectDynamic("box")))
        val outData: js.Array[ApexData] = cats.map(c => 
            ApData(x = c, y = r.selectDynamic(c).selectDynamic("outliers")))
        val data = js.Array(
            anon.Data(boxData).setType("boxPlot").setName("box"),
            anon.Data(outData).setType("scatter").setName("outliers"))

        chart.updateSeries(data)
        chart.asInstanceOf[js.Dynamic]._windowResize()
        
    private def markElement(e: HTMLElement) = 
        htmx.removeClass(e, "mark")
        e.offsetWidth;
        htmx.addClass(e, "mark")

    private val drlr = raw"drl_(\d{1,2})_(\d{1,2})".r
    private val categorySelectId = "catSelect"
    @JSExport
    // call back of selection changed
    def editDRLChanged(selectId: String) = 
        val select = document.getElementById(selectId).asInstanceOf[HTMLSelectElement]
        // println(s"$selectId(${select.selectedIndex}) => ${select.options(select.selectedIndex).label}")
        selectId match
            case `categorySelectId` =>
                htmx.ajax("GET", s"/c/tab/2?catid=${select.selectedIndex}", "#contents")
            case drlr(cid, sid) =>
                val dlabel = URIUtils.encodeURIComponent(select.options(select.selectedIndex).label)
                setAjaxHandler(drlChangeCB(select))
                xhttp.open("GET", s"/api/setdrl/$cid/$sid/$dlabel", true)
                xhttp.send()

    private def drlChangeCB(select: HTMLSelectElement)(e: Event) = 
        // val r = JSON.parse(xhttp.responseText)
        xhttp.responseText match
            case "success" =>
                markElement(select)
            case errMsg =>
                dom.window.alert(s"FAILED TO STORE, PLEASE TRY AGAIN\n$errMsg")

    @JSExport
    def updateDrlSummary() = 
        val select = document.getElementById(drlSummaryCategoryId).asInstanceOf[HTMLSelectElement]
        val categoryName = select.options(select.selectedIndex).label
        htmx.ajax("GET", s"/c/drlsummary/$categoryName/$intervalIndex", s"#$drlResultId")
