package net.maryknollrad.yader

object Constants:
    private val codeName = "Yet Another Dose Extraction and Reporting engine"
    
    // JS part
    val graphNoDataMsg = "No Data To Display"
    val activeTabColor = "bg-base-100"
    val inactiveTabColor = "bg-base-300"

    val modalMarkId = "modalMark"
    val trendGraphId = "trendgraph"
    val drlResultId = "drlStatResult"
    val drlSummaryCategoryId = "drlCategory"
    
    val queryIntervals = Seq("Day", "Week", "Month", "Year")
    val jobTabs = Seq("Graph", "DRL", "DRL Edit")