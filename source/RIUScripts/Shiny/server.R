## server.R ##
library(data.table)
library(DT)
library(googleVis)
library(htmltools)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(visNetwork)
library(plotly)
library(logging)
source("../RFM/findRFM.R")
source("../RFM/rfmMC.R")
source("../Clumpiness/clumpiness.R")
source("../LTV/findLTV.R")

basicConfig()
addHandler(writeToFile, 
           file = paste0(.GlobalEnv$logpath, 
                         format(Sys.time(), "%Y%m%d%H%M%S"),".log"))
						 
hideSettingTab <- function() {
  # Hide the Setting on the side bar using shinyjs
  shinyjs::hide(selector = "a[data-value='setting']")
}

printSessionDetail <- function(clientData) {
  detail <- paste(sep = "",
                  "protocol: ", clientData$url_protocol, "\n",
                  "hostname: ", clientData$url_hostname, "\n",
                  "pathname: ", clientData$url_pathname, "\n",
                  "port: ",     clientData$url_port,     "\n",
                  "search: ",   clientData$url_search,   "\n"
  )
  loginfo(paste("Session created.\n", detail, sep = ""))
}

getBu <- function(queryString) {
  names(queryString) <- tolower(names(queryString))
  if (is.null(queryString)) {
    loginfo("Set BU = 106 by defalut.")
    return("106")
  } else {
    if ("bu" %in% names(queryString)) {
      bu <- queryString[["bu"]]
      loginfo(paste0("Set BU = ", bu, "."))
      return(bu)
    } else {
      loginfo("Set BU = 106 by defalut.")
      return("106")
    }
  }
}

getReadTxn <- function(bu) {
  switch(as.character(bu),
         "106" = {
           source("../Utils/bauhaus.R")
           readtxn <- Bauhaus.readtxn
         },
         "107" = {
           source("../Utils/sevenFans.R")
           readtxn <- sevenFans.readtxn
         }
  )
  return(readtxn)
}

updateTxnData <- function(path, current, readtxn) {
  txnEnd <- current %m-% days(1)
  txnBegin <- format(txnEnd %m-% months(18), "%Y%m%d")
  txnEnd <- format(txnEnd, "%Y%m%d")
  return(readtxn(file = path, begin = txnBegin, end = txnEnd))
}

shinyServer(function(input, output, session) {
  hideSettingTab()

  presentButtonRv <- reactiveValues(noOfCustomerByStatsType = "F",
                                    avgStatsType = "R")
  presentCDataRv <- reactiveValues(cdata = session$clientData)
  presentSettingRv <- reactiveValues(bu = "106",
                                     txnPath = "http://127.0.0.1:8888",
                                     rdsPath = "../../RIURDS/",
									 current = as.Date("2018-08-31"))
                                     #current = Sys.Date())
  presentTxnDataRv <- reactiveValues(readtxn = NULL,
                                     txnData = data.table())
  presentRFMRv <- reactiveValues(rfmData = NULL,
                                 rfmScore = NULL,
                                 rfmSegment = NULL)
  presentRFMTransitionRv <- reactiveValues(mcSeq = NULL,
                                           mcCounts = NULL,
                                           transProb = NULL)
  presentLTVRv <- reactiveValues(customerSummary = data.table())
  
  isolate({
    printSessionDetail(presentCDataRv$cdata)
    presentSettingRv$bu <- getBu(getQueryString(session))
    presentTxnDataRv$readtxn <- getReadTxn(presentSettingRv$bu)
	if (presentSettingRv$bu == "107") {
		presentSettingRv$current <- Sys.Date()
	}
	loginfo("Getting transaction record.")
    # presentTxnDataRv$txnData <- updateTxnData(path = presentSettingRv$txnPath,
    #                                           current = presentSettingRv$current,
    #                                           readtxn = presentTxnDataRv$readtxn)
    rdsFilename <- paste(paste(presentSettingRv$bu, 
                         format(presentSettingRv$current, "%Y%m%d"), 
                         sep = "_"), ".rds", sep = "")
    loginfo(paste0("Getting RFM data (", rdsFilename,")."))
    presentRFMRv <- readRDS(paste(presentSettingRv$rdsPath, "RFM_", 
                                  rdsFilename, sep = ""))
    # presentRFMRv$rfmData <- readRFMfromURL(presentSettingRv$txnPath, 
    #                                        presentSettingRv$bu, 
    #                                        presentSettingRv$current %m-% days(1) %m-% months(12), 
    #                                        presentSettingRv$current %m-% days(1))
    # presentRFMRv$rfmScore <- rfmCompute(presentRFMRv$rfmData)
    # presentRFMRv$rfmSegment <- rfmSegmentation(presentRFMRv$rfmScore)
    # Check for column names
    if("MemberID" %in% colnames(presentRFMRv$rfmData)) {
      setnames(presentRFMRv$rfmData, "MemberID", "CustomerID")
    }
    presentRFMRv$customerSummary <- getRFMCustomerSummary(presentRFMRv$rfmData, 
                                                          presentRFMRv$rfmScore,
                                                          presentRFMRv$rfmSegment)
    
    presentRFMTransitionRv <- readRDS(paste(presentSettingRv$rdsPath, "RFM_MC_", 
                                            rdsFilename, sep = ""))
    presentLTVRv <- readRDS(paste(presentSettingRv$rdsPath, "LTV_", 
                                  rdsFilename, sep = ""))
  })
  
  periodChanged <- observeEvent(
    {
      session
    }, {
      # Hide the Gender button if Gender is not available in the dataset
      if (!("Gender" %in% colnames(presentRFMRv$rfmData))) {
        shinyjs::hide("avgStatBySex")
        updateCheckboxInput(session, "avgStatBySex", value = FALSE)
      }
      
      # update the rfmCharacteristicsSegmentSelector drop down menu
      updateSelectInput(session, "rfmCharacteristicsSegmentSelector",
                        label = "Segment:",
                        choices = c("All", 
                                    unique(presentRFMRv$rfmSegment$Segment)),
                        selected = "All")
      
      # Refresh Average Stats Title
      avgStatsTitle <- switch(presentButtonRv$avgStatsType,
                              "R" = "Average Recency (days)",
                              "F" = "Average Orders",
                              "M" = "Average Spending ($)")
      output$avgStatsTitle <- renderText(avgStatsTitle)
      
      # Plot Average Stats per Segment
      output$avgStatPlot <- renderGvis(
        plotAvgStatPerSegment(presentRFMRv$rfmData, presentRFMRv$rfmScore,
                              presentRFMRv$rfmSegment, 
                              presentButtonRv$avgStatsType,
                              input$avgStatBySex))
      
      # Refresh Number of Customers by Stats Title
      output$noOfCustomerByStatsTitle <- 
        if (presentButtonRv$noOfCustomerByStatsType == "F") {
          renderText("Number of Customers by Orders")
        } else {
          renderText("Number of Customers by Total Spending")
        }
      
      # Plot number of customer by Stats
      output$noOfCustomerByStats <- renderGvis(
        plotStatPerCustomer(presentRFMRv$rfmData, 
                            presentButtonRv$noOfCustomerByStatsType))
      
      # Plot number of customer by segment
      output$noOfCustomerPerSegmemtPie <- renderGvis(
        plotNoOfCustomersPerSegment(presentRFMRv$rfmData, 
                                    presentRFMRv$rfmSegment, 
                                    bySex = FALSE, interactive = TRUE))
      
      # Generate table of number of customers per segment
      output$noOfCustomerPerSegmemtTable <- renderDataTable({
        dt <- getNoOfCustomersPerSegment(presentRFMRv$rfmData, 
                                         presentRFMRv$rfmSegment, 
                                         FALSE, TRUE)
        cnames <- colnames(dt)
        selectedColTotal <- (cnames == "Number of Customers")
        colTotalFooter <- rep("", length(selectedColTotal))
        percentColIdx <- which(cnames == "Percentage")
        colTotalFooterJsCode <- paste("function(tfoot, data, start, end, display) ",
                                      "{ var api = this.api(), data;",
                                      "var cust_total = api.column(", 
                                      which(selectedColTotal) - 1,
                                      ").data().reduce( ",
                                      "function ( a, b ) {return a + b;} );",
                                      "$( api.column(", 
                                      which(selectedColTotal) - 1, ").footer()", 
                                      ").html('Total: ' + ", 
                                      "cust_total);", 
                                      "$( api.column(",
                                      percentColIdx - 1, ").footer()",
                                      ").html('Total: 100.00%'); }", sep = "")
        
        sketch <- htmltools::withTags(table(tableHeader(cnames), 
                                            tableFooter(colTotalFooter)))
        # contianer option is for column sum
        # RFM_SEGMENT is defined in ../RFM/findRFM.R
        # colTotalFooterJsCode is the JS code for column sum
        DT::datatable(dt, class = 'compact', rownames= FALSE, 
                      container = sketch,
                      options = list(lengthChange = FALSE, 
                                     pageLength = length(RFM_SEGMENT),  
                                     searching = FALSE, dom = 't', 
                                     ordering = TRUE,
                                     footerCallback = JS(colTotalFooterJsCode))
        ) %>% formatPercentage("Percentage", 2)
      })
      
      # Generate table of Customer RFM Summary
      output$rfmCustomerSummaryTable <- renderDataTable(
        presentRFMRv$customerSummary,
        class = 'compact', rownames= FALSE, filter = 'top',
        extensions = c('Scroller'),
        options = list(lengthChange = FALSE, deferRender = TRUE,
                       scrollY = 200, scroller = TRUE,
                       dom = 'frtip',
                       searchCols = list(NULL, NULL, NULL,
                                         NULL, NULL, NULL))
      )  # TODO: Possibility to merge with LTV?
      
      # Render the RFM transition probabilities
      output$rfmTransProb <- renderVisNetwork(
        plotRFMTransition(presentRFMTransitionRv$transProb))
      output$rfmTransProbLegend <- renderUI(HTML(paste(
        "<ol><li>", paste(RFM_SEGMENT, collapse = "</li><li>"), 
        "</li></ol>", sep = ""))
      )
      
      # # Render the RFM transition
      # output$rfmTransProb <- renderImage({
      #   list(src = normalizePath("./www/transProb.png"),
      #        contentType = 'image/png',
      #        width = 960,
      #        height = 640,
      #        alt = "RFM Transition Probabilities")
      # }, deleteFile = FALSE)
      
      # Render the RFM prediction
      output$rfmPrediction <- renderImage({
        list(src = normalizePath("./www/predicted_segment_movement.svg"),
             contentType = 'image/svg+xml',
             width = 960,
             height = 640,
             alt = "RFM Prediction")
      }, deleteFile = FALSE)
      
      # Render the RFM prediction performance
      output$rfmPredictionPerformance <- renderImage({
        list(src = normalizePath("./www/predicted_segment_movement_comparison.svg"),
             contentType = 'image/svg+xml',
             width = 960,
             height = 640,
             alt = "RFM Prediction Performance")
      }, deleteFile = FALSE)
      
      # Render the LTV per Segment
      # output$avgLTVperSegment <- renderImage({
      #   list(src = normalizePath("./www/LTV.svg"),
      #        contentType = 'image/svg+xml',
      #        width = 960,
      #        height = 640,
      #        alt = "Average Customer Lifetime Value by Segment")
      # }, deleteFile = FALSE)
      output$avgLTVperSegment <- renderGvis(
        plotLTVPerSegment(presentLTVRv$customerSummary,
                          presentRFMRv$rfmSegment, interactive = TRUE))
      
      # Render the Average Revenue per Segment
      output$avgRevenueperSegment <- renderImage({
        list(src = normalizePath("./www/avg_spending.svg"),
             contentType = 'image/svg+xml',
             width = 960,
             height = 640,
             alt = "Average Revenue per Transaction by Segment")
      }, deleteFile = FALSE)
      
      # Render Tracking Weekly Transactions
      output$btydTrackingTransaction <- renderImage({
        list(src = normalizePath("./www/BTYD_tracking.svg"),
             contentType = 'image/svg+xml',
             width = 960,
             height = 640,
             alt = "Tracking Weekly Transactions")
      }, deleteFile = FALSE)
      
      # Render the Average Number of Visit per Segment
      output$avgVisitperSegment <- renderImage({
        list(src = normalizePath("./www/BTYD_prediction.svg"),
             contentType = 'image/svg+xml',
             width = 960,
             height = 640,
             alt = "Average Number of Transactions by Segment")
      }, deleteFile = FALSE)
      
      # Render the price sensitivity
      output$priceSensitivity <- renderImage({
        list(src = normalizePath("./www/pricevolume.png"),
             contentType = 'image/png',
             width = 960,
             height = 640,
             alt = "Purchase Behaviour under Discount")
      }, deleteFile = FALSE)
      
      # Render table of Customer LTV Summary
      output$LTVCustomerSummaryTable <- renderDataTable({
        dt <- copy(presentLTVRv$customerSummary)
        # Change column names
        setnames(dt, c("CustomerID", "lifetimeValue", "pAlive"), 
                 c("Customer ID", "Customer Lifetime Value", "Probability of Alive"))
        DT::datatable(dt, class = 'compact', rownames= FALSE, filter = 'top',
                      extensions = c('Scroller'),
                      options = list(lengthChange = FALSE, deferRender = TRUE,
                                     scrollY = 200, scroller = TRUE,
                                     dom = 'frtip',
                                     searchCols = list(NULL, NULL, NULL,
                                                       NULL, NULL, NULL))
        ) %>% formatRound(c("Customer Lifetime Value", 
                            "Probability of Alive"), 2)
      })  # TODO: Possibility to merge with RFM?
      
      # Render Proportion of Population Aged 45-64
      output$populationAge <- renderImage({
        list(src = normalizePath("./www/Proportion of Population Aged 45-64.svg"),
             contentType = 'image/svg+xml',
             width = 960,
             height = 640,
             alt = "Proportion of Population Aged 45-64")
      }, deleteFile = FALSE)
      
      # Render Labour Force Participation Rate - Male
      output$labourForce <- renderImage({
        list(src = normalizePath("./www/Labour Force Participation Rate - Male.svg"),
             contentType = 'image/svg+xml',
             width = 960,
             height = 640,
             alt = "Labour Force Participation Rate - Male")
      }, deleteFile = FALSE)
      
      # Render Number of Rooms
      output$hotelRooms <- renderImage({
        list(src = normalizePath("./www/Number of Rooms.svg"),
             contentType = 'image/svg+xml',
             width = 960,
             height = 640,
             alt = "Number of Hotel Rooms")
      }, deleteFile = FALSE)
      
      # Render Total Number of Transactions by Month
      output$totalTxnByMonth <- renderImage({
        list(src = normalizePath("./www/tot_no_of_txn_by_month.svg"),
             contentType = 'image/svg+xml',
             width = 960,
             height = 640,
             alt = "Total Number of Transactions by Month")
      }, deleteFile = FALSE)
      
      # Render Customer Activity Analysis
      # output$customerActivity <- renderPlotly({
      #   selectedCustomer <- sample(unique(presentTxnDataRv$txnData$MemberID), 50)
      #   plotEventDetail(presentTxnDataRv$txnData, selectedCustomer)
      # })
      output$customerActivity <- renderGvis({
        # txn <- presentTxnDataRv$txnData
        # txn <- cbind(txn, align.time(as.POSIXlt(txn$Timestamp), n = 24*3600)) # 24 hrs
        # colnames(txn)[ncol(txn)] <- 'Date'
        # noOfTxn <- txn %>% group_by(Date) %>% 
        #   summarise(NoOfTxn = length(unique(OrderID)))
        # plotCalendarHeatmap(noOfTxn, "Date", "NoOfTxn", plotTitle)
        
        noOfTxn <- getNumberOfTxn(presentSettingRv$txnPath, 
                                  bu = presentSettingRv$bu, units = "date")
        plotTitle <- "Total Number of Transactions"
        plotCalendarHeatmap(noOfTxn, "Date", "value", plotTitle)
      })
      
      # # Render Store Activity Analysis
      # output$storeActivity <- renderPlotly({
      #   plotEventDetailByStore(presentRv$txnData)
      # })
    }
  )
  
  # Handler for rfmCharacteristicsSegmentSelector drop-down menu
  rfmCharacteristicsSegmentSelectorChanged <- observeEvent({
    input$rfmCharacteristicsSegmentSelector
    input$sbmenu
  }, {
    # Get selected segment
    rowsInfo <- input$rfmCharacteristicsSegmentSelector
    if (!is.null(rowsInfo) && !("All" %in% rowsInfo)) {
      segmentChar <- as.character(unique(presentRFMRv$customerSummary$Segment))
      selected <- unlist(input$rfmCharacteristicsSegmentSelector)
      selected <- (selected == segmentChar)
      
      # Plot number of customer by Stats
      output$noOfCustomerByStats <- renderGvis(
        plotStatPerCustomer(
          presentRFMRv$rfmData[
            CustomerID %in% presentRFMRv$rfmSegment[
              Segment %in% segmentChar[selected], CustomerID]
            ], presentButtonRv$noOfCustomerByStatsType))
    } else {
      # Plot number of customer by Stats
      output$noOfCustomerByStats <- renderGvis(
        plotStatPerCustomer(presentRFMRv$rfmData, 
                            presentButtonRv$noOfCustomerByStatsType))
    }
  })
  
  # Handler the noOfCustomerByStatsBtn button
  noOfCustomerByStatsBtnPressed <- observeEvent({
    input$noOfCustomerByStatsBtn
    input$sbmenu
  }, {
    presentButtonRv$noOfCustomerByStatsType <-
      switch(presentButtonRv$noOfCustomerByStatsType,
             "F" = "M",
             "M" = "F")
    # Refresh Number of Customers by Stats Title
    output$noOfCustomerByStatsTitle <- 
      if (presentButtonRv$noOfCustomerByStatsType == "F") {
        renderText("Number of Customers by Orders")
      } else {
        renderText("Number of Customers by Total Spending")
      }
    # Get selected rows
    rowsInfo <- input$rfmCharacteristicsSegmentSelector
    if (!is.null(rowsInfo) && !("All" %in% rowsInfo)) {
      segmentChar <- as.character(unique(presentRFMRv$customerSummary$Segment))
      selected <- unlist(input$rfmCharacteristicsSegmentSelector)
      selected <- (selected == segmentChar)
      
      # Plot number of customer by Stats
      output$noOfCustomerByStats <- renderGvis(
        plotStatPerCustomer(
          presentRFMRv$rfmData[
            CustomerID %in% presentRFMRv$rfmSegment[
              Segment %in% segmentChar[selected], CustomerID]
            ], presentButtonRv$noOfCustomerByStatsType))
    } else {
      # Plot number of customer by Stats
      output$noOfCustomerByStats <- renderGvis(
        plotStatPerCustomer(presentRFMRv$rfmData, 
                            presentButtonRv$noOfCustomerByStatsType))
    }
  })
  
  # Handler for Gender grouping for the plot of average stats per segment
  avgStatBySexChanged <- observeEvent({
    input$avgStatBySex
    input$sbmenu
  }, {
    output$avgStatPlot <- renderGvis(
      plotAvgStatPerSegment(presentRFMRv$rfmData, presentRFMRv$rfmScore,
                            presentRFMRv$rfmSegment, 
                            presentButtonRv$avgStatsType,
                            input$avgStatBySex))
  })
  
  # Handler the avgStatsBtn button
  avgStatsBtnPressed <- observeEvent({
    input$avgStatsBtn
    input$sbmenu
  }, {
    presentButtonRv$avgStatsType <-
      switch(presentButtonRv$avgStatsType,
             "R" = "F",
             "F" = "M",
             "M" = "R")
    avgStatsTitle <- switch(presentButtonRv$avgStatsType,
                            "R" = "Average Recency (days)",
                            "F" = "Average Orders",
                            "M" = "Average Spending ($)")
    output$avgStatsTitle <- renderText(avgStatsTitle)
    output$avgStatPlot <- renderGvis(
      plotAvgStatPerSegment(presentRFMRv$rfmData, presentRFMRv$rfmScore,
                            presentRFMRv$rfmSegment, 
                            presentButtonRv$avgStatsType,
                            input$avgStatBySex))
  })
  
  # Handler for downloading RFM analysis
  output$downloadRFM <- downloadHandler(
    filename = function() {
      paste('rfm-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      fwrite(x = presentRFMRv$customerSummary, file = con, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Handler for downloading LTV
  output$downloadLTV <- downloadHandler(
    filename = function() {
      paste('ltv-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      fwrite(x = presentLTVRv$customerSummary, file = con, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Handler on session ended
  session$onSessionEnded(function() {
    presentButtonRv <- NULL
    presentLTVRv <- NULL
    presentRFMTransitionRv <- NULL
    presentRFMRv <- NULL
    presentTxnDataRv <- NULL
    presentSettingRv <- NULL
    presentCDataRv <- NULL
    gc()
  })
})