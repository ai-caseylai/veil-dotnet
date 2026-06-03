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

      # Find the actual RDS file regardless of date
      findRDS <- function(path, prefix, bu) {
        pattern <- paste0("^", prefix, bu, "_.*\\.rds$")
        files <- list.files(path, pattern = pattern, full.names = TRUE)
        if (length(files) == 0) stop("No RDS file found for ", prefix, bu, " in ", path)
        # Sort by file modification time descending — pick the most recent
        files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
        return(files[1])
      }
      rfmPath <- findRDS(presentSettingRv$rdsPath, "RFM_", presentSettingRv$bu)
      message("[DEBUG 5/9] rfmPath = ", rfmPath, " (exists=", file.exists(rfmPath), ")")
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

      mcPath <- findRDS(presentSettingRv$rdsPath, "RFM_MC_", presentSettingRv$bu)
      message("[DEBUG 9/9] Loading MC: ", mcPath, " (exists=", file.exists(mcPath), ")")
      mclist <- readRDS(mcPath)
      presentRFMTransitionRv$mcSeq <- mclist$mcSeq
      presentRFMTransitionRv$mcCounts <- mclist$mcCounts
      presentRFMTransitionRv$transProb <- mclist$transProb

      ltvPath <- findRDS(presentSettingRv$rdsPath, "LTV_", presentSettingRv$bu)
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

  # =========================================================================
  # WHAT-IF ANALYSIS — real-time parameter adjustment
  # =========================================================================
  wiRv <- reactiveValues(
    rfmScore = NULL,
    rfmSegment = NULL,
    avgStatsType = "R",
    custDistType = "F"
  )

  # When "Apply" is clicked, re-run rfmCompute + rfmSegmentation
  observeEvent(input$wi_apply, {
    req(presentRFMRv$rfmData)
    tryCatch({
      wiRv$rfmScore <- rfmCompute(presentRFMRv$rfmData,
                                  max.score = input$wi_maxScore,
                                  recencyWeight = input$wi_recencyWt,
                                  frequencyWeight = input$wi_frequencyWt,
                                  monetaryWeight = input$wi_monetaryWt,
                                  thres = input$wi_thres,
                                  minTxn = input$wi_minTxn)
      wiRv$rfmSegment <- rfmSegmentation(wiRv$rfmScore)
      message("[WHATIF] Recalculated: ", nrow(wiRv$rfmSegment), " segments")
    }, error = function(e) {
      message("[WHATIF ERROR] ", conditionMessage(e))
    })
  }, ignoreNULL = FALSE)

  # Initialize with default values on first load
  observe({
    req(presentRFMRv$rfmScore, presentRFMRv$rfmSegment)
    if (is.null(wiRv$rfmScore)) {
      wiRv$rfmScore <- presentRFMRv$rfmScore
      wiRv$rfmSegment <- presentRFMRv$rfmSegment
    }
  })

  # -- Segment table --
  output$wi_segmentCount <- renderText({
    req(wiRv$rfmSegment)
    paste0("Total: ", nrow(wiRv$rfmSegment), " customers in ",
           length(unique(wiRv$rfmSegment$Segment)), " segments")
  })

  output$wi_segmentTable <- renderDataTable({
    req(wiRv$rfmSegment, presentRFMRv$rfmData)
    dt <- getNoOfCustomersPerSegment(presentRFMRv$rfmData, wiRv$rfmSegment, FALSE, TRUE)
    cnames <- colnames(dt)
    selectedColTotal <- (cnames == "Number of Customers")
    percentColIdx <- which(cnames == "Percentage")
    colTotalFooter <- rep("", length(selectedColTotal))
    colTotalFooterJsCode <- paste(
      "function(tfoot, data, start, end, display) {",
      "var api = this.api(), data;",
      "var cust_total = api.column(", which(selectedColTotal) - 1,
      ").data().reduce(function(a,b){return a+b;});",
      "$(api.column(", which(selectedColTotal) - 1, ").footer()).html('Total: ' + cust_total);",
      "$(api.column(", percentColIdx - 1, ").footer()).html('Total: 100.00%');",
      "}", sep = "")
    sketch <- htmltools::withTags(table(tableHeader(cnames), tableFooter(colTotalFooter)))
    DT::datatable(dt, class = 'compact', rownames = FALSE, container = sketch,
                  options = list(lengthChange = FALSE, pageLength = 11,
                                 searching = FALSE, dom = 't', ordering = TRUE,
                                 footerCallback = JS(colTotalFooterJsCode))
    ) %>% formatPercentage("Percentage", 2)
  })

  # -- Segment pie chart --
  output$wi_segmentPie <- renderGvis({
    req(wiRv$rfmSegment, presentRFMRv$rfmData)
    plotNoOfCustomersPerSegment(presentRFMRv$rfmData, wiRv$rfmSegment, bySex = FALSE, interactive = TRUE)
  })

  # -- Average stats --
  output$wi_avgStatsTitle <- renderText({
    switch(wiRv$avgStatsType, "R" = "Average Recency (days)",
           "F" = "Average Orders", "M" = "Average Spending ($)")
  })

  output$wi_avgStatPlot <- renderGvis({
    req(wiRv$rfmSegment, presentRFMRv$rfmData, wiRv$rfmScore)
    plotAvgStatPerSegment(presentRFMRv$rfmData, wiRv$rfmScore, wiRv$rfmSegment,
                          wiRv$avgStatsType, input$wi_avgBySex)
  })

  observeEvent(input$wi_avgStatsBtn, {
    wiRv$avgStatsType <- switch(wiRv$avgStatsType, "R" = "F", "F" = "M", "M" = "R")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # -- Customer distribution --
  output$wi_custDistTitle <- renderText({
    if (wiRv$custDistType == "F") "Number of Customers by Orders"
    else "Number of Customers by Total Spending"
  })

  output$wi_custDistCharts <- renderUI({
    req(wiRv$rfmSegment, presentRFMRv$rfmData)
    segments <- unique(wiRv$rfmSegment$Segment)
    chart_list <- lapply(seq_along(segments), function(i) {
      seg <- segments[i]
      segCustomers <- wiRv$rfmSegment[Segment == seg, CustomerID]
      segData <- presentRFMRv$rfmData[CustomerID %in% segCustomers]
      if (nrow(segData) > 0) {
        chart <- plotStatPerCustomer(segData, wiRv$custDistType)
        tagList(
          tags$h4(paste0(i, ". ", seg, " (", nrow(segData), " customers)")),
          HTML(chart$html$chart),
          tags$hr()
        )
      }
    })
    do.call(tagList, chart_list)
  })

  observeEvent(input$wi_custDistBtn, {
    wiRv$custDistType <- switch(wiRv$custDistType, "F" = "M", "M" = "F")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # -- Regenerate handler --
  output$wi_regenerateStatus <- renderText({
    if (is.null(wiRv$regenStatus)) return("Ready.")
    wiRv$regenStatus
  })

  observeEvent(input$wi_regenerate, {
    wiRv$regenStatus <- "Regenerating 1000 customers... (this takes ~15 sec)"
    message("[WHATIF] Starting regeneration...")
    # Run in background via system()
    system("cd /Users/perry/Documents/veil; Rscript generate_synthetic_customers.R > /tmp/veil_regen.log 2>&1", wait = FALSE)
    # Poll for completion
    showNotification("Regeneration started — reloading in 20 seconds...", type = "warning", duration = 5)
    # Invalidate to reload after delay
    invalidateLater(20000)
    wiRv$regenStatus <- "Running..."
  })

  # Auto-reload RDS after regeneration
  observe({
    invalidateLater(25000)
    req(wiRv$regenStatus)
    if (wiRv$regenStatus == "Running...") {
      tryCatch({
        # Reload the RDS files
        rfmPath <- findRDS(presentSettingRv$rdsPath, "RFM_", presentSettingRv$bu)
        rfmlist <- readRDS(rfmPath)
        presentRFMRv$rfmData <- rfmlist$rfmData
        presentRFMRv$rfmScore <- rfmlist$rfmScore
        presentRFMRv$rfmSegment <- rfmlist$rfmSegment
        if("MemberID" %in% colnames(presentRFMRv$rfmData)) {
          setnames(presentRFMRv$rfmData, "MemberID", "CustomerID")
        }

        ltvPath <- findRDS(presentSettingRv$rdsPath, "LTV_", presentSettingRv$bu)
        ltvlist <- readRDS(ltvPath)
        presentLTVRv$customerSummary <- ltvlist$customerSummary

        mcPath <- findRDS(presentSettingRv$rdsPath, "RFM_MC_", presentSettingRv$bu)
        mclist <- readRDS(mcPath)
        presentRFMTransitionRv$mcSeq <- mclist$mcSeq
        presentRFMTransitionRv$mcCounts <- mclist$mcCounts
        presentRFMTransitionRv$transProb <- mclist$transProb

        # Reset what-if to match new data
        wiRv$rfmScore <- presentRFMRv$rfmScore
        wiRv$rfmSegment <- presentRFMRv$rfmSegment

        wiRv$regenStatus <- paste0("Done! Reloaded at ", Sys.time())
        showNotification("Regeneration complete — data reloaded!", type = "success", duration = 8)
      }, error = function(e) {
        wiRv$regenStatus <- paste("Error:", conditionMessage(e))
      })
    }
  })

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