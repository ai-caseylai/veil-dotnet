## server.R ##
library(data.table)
library(dplyr)
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
    bu <- "106"
  } else {
    if ("bu" %in% names(queryString)) {
      bu <- queryString[["bu"]]
    } else {
      bu <- "106"
    }
  }
  if (bu == "106" && exists("bu", envir = .GlobalEnv)) {
    bu <- as.character(.GlobalEnv$bu)
    message("[DEBUG] BU from .GlobalEnv: ", bu)
  }
  loginfo(paste0("Set BU = ", bu, "."))
  return(bu)
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
    tryCatch({
      message("[DEBUG 1/9] Session start, url_search=", session$clientData$url_search)
      printSessionDetail(presentCDataRv$cdata)
      presentSettingRv$bu <- getBu(getQueryString(session))
      message("[DEBUG 2/9] BU = ", presentSettingRv$bu)
      presentTxnDataRv$readtxn <- getReadTxn(presentSettingRv$bu)
      message("[DEBUG 3/9] readtxn loaded OK")
      if (presentSettingRv$bu == "107") {
        presentSettingRv$current <- Sys.Date()
      }
      message("[DEBUG 4/9] current = ", presentSettingRv$current, ", rdsPath = ", presentSettingRv$rdsPath)

      rdsFilename <- paste(paste(presentSettingRv$bu,
                           format(presentSettingRv$current, "%Y%m%d"),
                           sep = "_"), ".rds", sep = "")
      message("[DEBUG 5/9] rdsFilename = ", rdsFilename)

      rfmPath <- paste(presentSettingRv$rdsPath, "RFM_", rdsFilename, sep = "")
      message("[DEBUG 6/9] Loading RFM: ", rfmPath, " (exists=", file.exists(rfmPath), ")")
      rfmlist <- readRDS(rfmPath)
      presentRFMRv$rfmData <- rfmlist$rfmData
      presentRFMRv$rfmScore <- rfmlist$rfmScore
      presentRFMRv$rfmSegment <- rfmlist$rfmSegment
      message("[DEBUG 7/9] RFM loaded: rfmData=", nrow(presentRFMRv$rfmData), " rows, cols=", paste(colnames(presentRFMRv$rfmData), collapse=","))

      if("MemberID" %in% colnames(presentRFMRv$rfmData)) {
        setnames(presentRFMRv$rfmData, "MemberID", "CustomerID")
      }
      presentRFMRv$customerSummary <- getRFMCustomerSummary(presentRFMRv$rfmData,
                                                            presentRFMRv$rfmScore,
                                                            presentRFMRv$rfmSegment)
      message("[DEBUG 8/9] customerSummary: ", nrow(presentRFMRv$customerSummary), " rows")

      mcPath <- paste(presentSettingRv$rdsPath, "RFM_MC_", rdsFilename, sep = "")
      message("[DEBUG 9/9] Loading MC: ", mcPath, " (exists=", file.exists(mcPath), ")")
      mclist <- readRDS(mcPath)
      presentRFMTransitionRv$mcSeq <- mclist$mcSeq
      presentRFMTransitionRv$mcCounts <- mclist$mcCounts
      presentRFMTransitionRv$transProb <- mclist$transProb

      ltvPath <- paste(presentSettingRv$rdsPath, "LTV_", rdsFilename, sep = "")
      message("[DEBUG 10/10] Loading LTV: ", ltvPath, " (exists=", file.exists(ltvPath), ")")
      ltvlist <- readRDS(ltvPath)
      presentLTVRv$customerSummary <- ltvlist$customerSummary

      message("[DEBUG] ========== ALL INIT DONE - Session ready ==========")
    }, error = function(e) {
      message("[DEBUG FATAL] Init error: ", conditionMessage(e))
      traceback()
    })
  })

  periodChanged <- observeEvent(
    {
      session
    }, {
      message("[DEBUG EVENT] periodChanged fired")
      tryCatch({

        output$debugStatus <- renderText({
          paste(c(
            paste0("BU: ", presentSettingRv$bu),
            paste0("Current: ", presentSettingRv$current),
            paste0("RFM data rows: ", nrow(presentRFMRv$rfmData)),
            paste0("RFM segments: ", length(unique(presentRFMRv$rfmSegment$Segment))),
            paste0("customerSummary rows: ", nrow(presentRFMRv$customerSummary)),
            paste0("LTV rows: ", nrow(presentLTVRv$customerSummary)),
            paste0("transProb dim: ", paste(dim(presentRFMTransitionRv$transProb), collapse="x")),
            paste0("Time: ", Sys.time())
          ), collapse = "\n")
        })
        # Hide the Gender button if Gender is not available in the dataset
        if (!("Gender" %in% colnames(presentRFMRv$rfmData))) {
          shinyjs::hide("avgStatBySex")
          updateCheckboxInput(session, "avgStatBySex", value = FALSE)
        }

        # Refresh Average Stats Title
        avgStatsTitle <- switch(presentButtonRv$avgStatsType,
                                "R" = "Average Recency (days)",
                                "F" = "Average Orders",
                                "M" = "Average Spending ($)")
        output$avgStatsTitle <- renderText(avgStatsTitle)

        # Plot Average Stats per Segment
        message("[DEBUG EVENT] Plotting avgStatPlot...")
        output$avgStatPlot <- renderGvis(
          plotAvgStatPerSegment(presentRFMRv$rfmData, presentRFMRv$rfmScore,
                                presentRFMRv$rfmSegment,
                                presentButtonRv$avgStatsType,
                                input$avgStatBySex))
        message("[DEBUG EVENT] avgStatPlot done")

        # Refresh Number of Customers by Stats Title
        output$noOfCustomerByStatsTitle <-
          if (presentButtonRv$noOfCustomerByStatsType == "F") {
            renderText("Number of Customers by Orders")
          } else {
            renderText("Number of Customers by Total Spending")
          }

        # Plot number of customer by Stats — ALL segments stacked
        message("[DEBUG EVENT] Plotting noOfCustomerByStatsAllSegments...")
        output$noOfCustomerByStatsAllSegments <- renderUI({
          segments <- unique(presentRFMRv$rfmSegment$Segment)
          chart_list <- lapply(seq_along(segments), function(i) {
            seg <- segments[i]
            segCustomers <- presentRFMRv$rfmSegment[Segment == seg, CustomerID]
            segData <- presentRFMRv$rfmData[CustomerID %in% segCustomers]
            if (nrow(segData) > 0) {
              chart <- plotStatPerCustomer(segData, presentButtonRv$noOfCustomerByStatsType)
              tagList(
                tags$h4(paste0(i, ". ", seg, " (", nrow(segData), " customers)")),
                HTML(chart$html$chart),
                tags$hr()
              )
            }
          })
          do.call(tagList, chart_list)
        })
        message("[DEBUG EVENT] noOfCustomerByStatsAllSegments done")

        # Plot number of customer by segment
        message("[DEBUG EVENT] Plotting noOfCustomerPerSegmemtPie...")
        output$noOfCustomerPerSegmemtPie <- renderGvis(
          plotNoOfCustomersPerSegment(presentRFMRv$rfmData,
                                      presentRFMRv$rfmSegment,
                                      bySex = FALSE, interactive = TRUE))
        message("[DEBUG EVENT] Pie done")

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
        )

        # Render the RFM transition probabilities
        message("[DEBUG EVENT] Plotting rfmTransProb visNetwork...")
        output$rfmTransProb <- renderVisNetwork(
          plotRFMTransition(presentRFMTransitionRv$transProb))
        output$rfmTransProbLegend <- renderUI(HTML(paste(
          "<ol><li>", paste(RFM_SEGMENT, collapse = "</li><li>"),
          "</li></ol>", sep = ""))
        )
        message("[DEBUG EVENT] rfmTransProb done")

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
        message("[DEBUG EVENT] Plotting LTV per segment...")
        output$avgLTVperSegment <- renderGvis(
          plotLTVPerSegment(presentLTVRv$customerSummary,
                            presentRFMRv$rfmSegment, interactive = TRUE))
        message("[DEBUG EVENT] LTV done")

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
        })

        # Render static images
        output$populationAge <- renderImage({
          list(src = normalizePath("./www/Proportion of Population Aged 45-64.svg"),
               contentType = 'image/svg+xml',
               width = 960,
               height = 640,
               alt = "Proportion of Population Aged 45-64")
        }, deleteFile = FALSE)

        output$labourForce <- renderImage({
          list(src = normalizePath("./www/Labour Force Participation Rate - Male.svg"),
               contentType = 'image/svg+xml',
               width = 960,
               height = 640,
               alt = "Labour Force Participation Rate - Male")
        }, deleteFile = FALSE)

        output$hotelRooms <- renderImage({
          list(src = normalizePath("./www/Number of Rooms.svg"),
               contentType = 'image/svg+xml',
               width = 960,
               height = 640,
               alt = "Number of Hotel Rooms")
        }, deleteFile = FALSE)

        output$totalTxnByMonth <- renderImage({
          list(src = normalizePath("./www/tot_no_of_txn_by_month.svg"),
               contentType = 'image/svg+xml',
               width = 960,
               height = 640,
               alt = "Total Number of Transactions by Month")
        }, deleteFile = FALSE)

        # Render Customer Activity Analysis
        output$customerActivity <- renderGvis({
          noOfTxn <- getNumberOfTxn(presentSettingRv$txnPath,
                                    bu = presentSettingRv$bu, units = "date")
          plotTitle <- "Total Number of Transactions"
          plotCalendarHeatmap(noOfTxn, "Date", "value", plotTitle)
        })

        message("[DEBUG EVENT] ========== periodChanged COMPLETE ==========")
      }, error = function(e) {
        message("[DEBUG EVENT FATAL] Error in periodChanged: ", conditionMessage(e))
        traceback()
      })
    }
  )

  # Handler the noOfCustomerByStatsBtn button — refresh ALL segment charts
  noOfCustomerByStatsBtnPressed <- observeEvent(
    input$noOfCustomerByStatsBtn, {
    presentButtonRv$noOfCustomerByStatsType <-
      switch(presentButtonRv$noOfCustomerByStatsType,
             "F" = "M",
             "M" = "F")
    output$noOfCustomerByStatsTitle <-
      if (presentButtonRv$noOfCustomerByStatsType == "F") {
        renderText("Number of Customers by Orders")
      } else {
        renderText("Number of Customers by Total Spending")
      }
    # Re-render all segment charts
    output$noOfCustomerByStatsAllSegments <- renderUI({
      segments <- unique(presentRFMRv$rfmSegment$Segment)
      chart_list <- lapply(seq_along(segments), function(i) {
        seg <- segments[i]
        segCustomers <- presentRFMRv$rfmSegment[Segment == seg, CustomerID]
        segData <- presentRFMRv$rfmData[CustomerID %in% segCustomers]
        if (nrow(segData) > 0) {
          chart <- plotStatPerCustomer(segData, presentButtonRv$noOfCustomerByStatsType)
          tagList(
            tags$h4(paste0(i, ". ", seg, " (", nrow(segData), " customers)")),
            HTML(chart$html$chart),
            tags$hr()
          )
        }
      })
      do.call(tagList, chart_list)
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Handler for Gender grouping for the plot of average stats per segment
  avgStatBySexChanged <- observeEvent(
    input$avgStatBySex, {
    output$avgStatPlot <- renderGvis(
      plotAvgStatPerSegment(presentRFMRv$rfmData, presentRFMRv$rfmScore,
                            presentRFMRv$rfmSegment,
                            presentButtonRv$avgStatsType,
                            input$avgStatBySex))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Handler the avgStatsBtn button
  avgStatsBtnPressed <- observeEvent(
    input$avgStatsBtn, {
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
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

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