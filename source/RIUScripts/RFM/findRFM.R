library(data.table)
library(dplyr)
library(lubridate)
library(googleVis)
library(ggplot2)
library(scales)
library(ggrepel)
library(forcats)
source("../Utils/master.R")
source("../Visualizer/graphics.R")

RFM_SEGMENT <- c("Best Customers","Loyal Customers","Potential Loyalist",
                 "Low-spending Active Loyal Customers","High-spending New Customers",
                 "Almost Lost Customers","Churned Best Customers","Customers Needing Attention",
                 "About to Sleep Customers","Hibernating Customers","Lost Cheap Customers")

readRFMfromURL <- function(url, bu, begin = NULL, end = NULL) {
  if (any(is.null(c(begin, end)))) {
    current <- Sys.time() - days(1)
    end <- format(current, "%Y%m%d")
    begin <- format(current - years(1), "%Y%m%d")
  }
  api_url <- getUrl(address = url, type = "rfm", bu = bu, begin = begin, end = end)
  return(getRFMData(api_url))
}

getRFMData <- function(txnData, sumTotal = FALSE) {
  # This function prepares data to calculate RFM scores
  # Requirements: 'data.table'
  #
  # Args:
  #   txnData: A data.table object
  #            (required column: 'Timestamp', Transaction datetime in character
  #                              'TotalPrice', Total amount of each transaction
  #                              'OrderID', Transaction ID
  #                              'MemberID', UserID).
  #   sumTotal: If TRUE, compute each transaction total amount manualy.
  #
  # Output:
  #   A data.table object
  if (is.data.table(txnData)) {
    if (any(class(txnData$Timestamp) != "POSIXct")) {
      txnData$Timestamp <- as.POSIXct(txnData$Timestamp)
    }
    
    priceName <- "TotalPrice"
    if ("NetPrice" %in% names(txnData)) {
      priceName <- "NetPrice"
    }
    
    if (sumTotal) {
      # aggregated the total amount for each transaction 
      rfmData <- txnData[,.(TxnAmt = sum(get(priceName))), 
                         by = .(OrderID,MemberID,Timestamp)]
    } else {
      rfmData <- unique(txnData[,.(TxnAmt = get(priceName)), 
                                by = .(OrderID,MemberID,Timestamp)])
    }
    
    # remove the duplicated OrderID in the txnData
    duplicatedOrderID <- rfmData[duplicated(rfmData$OrderID),]$OrderID
    rfmData <- rfmData[!OrderID %in% duplicatedOrderID,]
    
    if (any(class(rfmData$Timestamp) != "Date")) {
      rfmData$Timestamp <- as.Date(rfmData$Timestamp)
    }
    
    colnames(rfmData) <- c("TransNo","CustomerID","DateofPurch","Amount")
    rfmData$TransNo <- as.character(rfmData$TransNo)
    rfmData$CustomerID <- as.character(rfmData$CustomerID)
    
    # Compute the statistics for RFM scroes calculation.
    rfmData <- rfmData[,.(MeanMoneyValue = mean(Amount),
                          LastTxnDate = max(DateofPurch),
                          NoOfTxn = length(TransNo),
                          TotalSpending = sum(Amount)), by = CustomerID]
  } else if (is.character(txnData)){
    rfmData <- read.url(txnData)
    rfmData$LastTxnDate <- as.Date(rfmData$LastTxnDate)
    if ("MemberID" %in% colnames(rfmData)) {
      setnames(rfmData, "MemberID", "CustomerID")
    }
    rfmData$CustomerID <- as.character(rfmData$CustomerID)
  } else {
    stop("The argument txnData is invalid.")
  }
  # lastTxnDate is the last day in the data set
  latestTxnDate <- max(rfmData$LastTxnDate)
  rfmData <- rfmData[, DaySinceLastTxn := as.numeric(latestTxnDate - LastTxnDate)]
  if ("Gender" %in% colnames(rfmData)) {
    setGender <- function(x) {
      x[x == "0.0" | x == 0] <- "NA"
      x[x == "1.0" | x == 1] <- "Male"
      x[x == "2.0" | x == 2] <- "Female"
      return(x)
    }
    rfmData <- rfmData[, Gender := setGender(Gender)]
    rfmData <- rfmData[, .(CustomerID, Gender, DaySinceLastTxn, NoOfTxn, MeanMoneyValue, TotalSpending)]
  } else {
    rfmData <- rfmData[, .(CustomerID, DaySinceLastTxn, NoOfTxn, MeanMoneyValue, TotalSpending)]
  }
  setkey(rfmData, CustomerID)
  return(rfmData)
}

rfmCompute <- function(rfmData, max.score = 5, recencyWeight = 4,
                    frequencyWeight = 4, monetaryWeight = 4,
                    thres = TRUE, minTxn = 2){
  # This function compute the RFM scores, weighted RFM scores.
  # Requirements: R (version >= 3.3.3), 'data.table', 'dplyr'
  #
  # Args:
  #   rfmData: A data.table object generated from 'getRFMData' function
  #   thres is: If TRUE, the frequency (F) score of a customer with 
  #             NoofTxn <= <minTxn> is 1. 
  #             The remaining will be 2, 3, ..., max.score, 
  #             according to its percentile.
  #
  # Output:
  #   A data.table object
  
  # Compute the percentile for RFM scores calculation, requires dplyr.
  rfmScore <- rfmData[, .(CustomerID, NoOfTxn, RecencyPercentile = percent_rank(-DaySinceLastTxn),
                          FrequencyPercentile = percent_rank(NoOfTxn),
                          MonetaryPercentile = percent_rank(MeanMoneyValue))]
  
  individualScore <- function (percentile)
  {
    # This function assign an integer score from the percentile
    percentiles <- seq(0, 1, length.out = max.score + 1)[-1]
    # findInterval function requires R (version >= 3.3.3)
    return(findInterval(percentile, percentiles, left.open = TRUE) + 1)
  }
  
  # Compute the recency score
  rfmScore <- rfmScore[,RecencyScore := individualScore(RecencyPercentile),by = .(CustomerID)]
  
  # Compute the frequency (F) score
  if(thres){
    # If the thres is TRUE, then the frequency (F) score of a customer with 
    # NoofTxn <= <minTxn> is 1. The remaining will be 2, 3, ..., max.score, 
    # according to its percentile.
    rfmScore <- rfmScore[,FrequencyScore := rankingFunc(rfmScore$NoOfTxn, minTxn, max.score-1)]
  } else{
    rfmScore <- rfmScore[,FrequencyScore := individualScore(FrequencyPercentile),by = .(CustomerID)]
  }
  
  # Compute monetary (M) score
  rfmScore <- rfmScore[,MonetaryScore := individualScore(MonetaryPercentile), by = .(CustomerID)]
  
  # Compute weighted score
  rfmScore <- rfmScore[,c("RecencyWeightedScore","FrequencyWeightedScore","MonetaryWeightedScore") :=
                       list(RecencyScore*recencyWeight, FrequencyScore*frequencyWeight, 
                            MonetaryScore*monetaryWeight)]
  # Compute sum of scores
  rfmScore <- rfmScore[,FinalScore := RecencyWeightedScore + FrequencyWeightedScore + MonetaryWeightedScore]
  
  # Compute sum of weighted scores
  rfmScore <- rfmScore[,FinalWeightedScore := FinalScore/(recencyWeight+frequencyWeight+monetaryWeight)]
  rfmScore <- rfmScore[,.(CustomerID, RecencyScore, FrequencyScore, MonetaryScore, FinalScore,
                         RecencyWeightedScore, FrequencyWeightedScore, MonetaryWeightedScore,
                         FinalWeightedScore)]
  setkey(rfmScore, CustomerID)
  return(rfmScore)
}

rfmSegmentation <- function(rfmScore) {
  # This function segmentate the customer by RFM score
  # Requirements: 'data.table'
  #
  # Args:
  #   rfmData: A data.table object generated from 'rfmCompute' function
  # Output:
  #   A data.table object
  #
  # Remark:
  #   RFM_SEGMENT: Global variable defined at findRFM.R
  
  rfmSegment <- rfmScore[, .(CustomerID, RecencyScore, FrequencyScore, MonetaryScore)]
  rfmSegment <- rfmSegment[, Segment := character()]
  
  rfmSegment[RecencyScore >= 3 & FrequencyScore >= 3 & 
                MonetaryScore >= 3,]$Segment <- RFM_SEGMENT[3]
  rfmSegment[RecencyScore >= 4 & FrequencyScore >= 4 & 
                MonetaryScore >= 4,]$Segment <- RFM_SEGMENT[2]
  rfmSegment[RecencyScore == 5 & FrequencyScore == 5 & 
                MonetaryScore == 5,]$Segment <- RFM_SEGMENT[1]
  
  rfmSegment[RecencyScore >= 4 & FrequencyScore <= 2 & 
                MonetaryScore >= 4,]$Segment <- RFM_SEGMENT[5]
  rfmSegment[RecencyScore >= 4 & FrequencyScore >= 4 & 
                MonetaryScore <= 2,]$Segment <- RFM_SEGMENT[4]
  
  rfmSegment[RecencyScore == 1 & FrequencyScore >= 4 & 
                MonetaryScore >= 4,]$Segment <- RFM_SEGMENT[7]
  rfmSegment[RecencyScore == 2 & FrequencyScore >= 4 & 
                MonetaryScore >= 4,]$Segment <- RFM_SEGMENT[6]
  rfmSegment[RecencyScore == 3 & FrequencyScore >= 4 & 
                MonetaryScore >= 4,]$Segment <- RFM_SEGMENT[6]
  
  
  rfmSegment[RecencyScore <= 3 & FrequencyScore <= 3 & 
                MonetaryScore <= 3,]$Segment <- RFM_SEGMENT[9]
  rfmSegment[RecencyScore <= 2 & FrequencyScore <= 2 & 
                MonetaryScore <= 2,]$Segment <- RFM_SEGMENT[10]
  rfmSegment[RecencyScore == 1 & FrequencyScore == 1 & 
                MonetaryScore == 1,]$Segment <- RFM_SEGMENT[11]
  
  rfmSegment[is.na(Segment),]$Segment <- RFM_SEGMENT[8]
  setkey(rfmSegment, CustomerID)
  return(rfmSegment[,.(CustomerID, Segment)])
}

getRFMResults <- function(rfmData, rfmScore, rfmSegment, weighted = FALSE) {
  # This function get the join of rfmData, rfmScore and rfmSegment
  # <weighted> means return the weighted scores.
  if (any(is.null(c(key(rfmData),key(rfmScore),key(rfmSegment))))) {
    # Check if key is set.
    setkey(rfmData, CustomerID)
    setkey(rfmScore, CustomerID)
    setkey(rfmSegment, CustomerID)
  }
  rfmResults <- rfmData[rfmScore, nomatch=0]
  rfmResults <- rfmResults[rfmSegment, nomatch=0]
  if(weighted) {
    return(rfmResults)
  } else {
    if ("Gender" %in% colnames(rfmResults)) {
      return(rfmResults[, .(CustomerID, Segment, Gender,
                            DaySinceLastTxn, NoOfTxn, TotalSpending,
                            RecencyScore, FrequencyScore, MonetaryScore)])
    } else {
      return(rfmResults[, .(CustomerID, Segment,
                            DaySinceLastTxn, NoOfTxn, TotalSpending,
                            RecencyScore, FrequencyScore, MonetaryScore)])
    }
  }
}

getNoOfCustomersPerSegment <- function(rfmData, rfmSegment, bySex = TRUE, 
                                       proportion = FALSE) {
  # This function get the number of customers in each segments.
  if ("Gender" %in% colnames(rfmData)) {
    rfmSegment <- rfmSegment[unique(rfmData[,.(CustomerID, Gender)]), nomatch = 0]
  } else {
    bySex <- FALSE
  }
  if (bySex) {
    noOfCustomerPerSegment <- rfmSegment[, .(NoOfCustomers = length(CustomerID)), 
                                         by = .(Segment, Gender)]
    colnames(noOfCustomerPerSegment) <- c("Segment", "Gender", "Number of Customers")
  } else {
    noOfCustomerPerSegment <- rfmSegment[, .(NoOfCustomers = length(CustomerID)), by = Segment]
    colnames(noOfCustomerPerSegment) <- c("Segment", "Number of Customers")
  }
  noOfCustomerPerSegment$Segment <- factor(noOfCustomerPerSegment$Segment, 
                                           levels = RFM_SEGMENT, ordered = TRUE)
  if (proportion) {
    num2prop <- function(x, total) {
      output <- x / total
      return(output)
    }
    noOfCustomerPerSegment <- noOfCustomerPerSegment[, Proportion := num2prop(`Number of Customers`, 
                                                                              sum(`Number of Customers`))]
    cnames <- colnames(noOfCustomerPerSegment)
    cnames <- c(cnames[seq(length(cnames)-1)], "Percentage")
    colnames(noOfCustomerPerSegment) <- cnames
  }
  setorderv(noOfCustomerPerSegment, "Segment")
  return(noOfCustomerPerSegment)
}

getRFMCustomerSummary <- function(rfmData, rfmScore, rfmSegment) {
  rfmResults <- getRFMResults(rfmData, rfmScore, rfmSegment)
  rfmSummary <- rfmResults[, .(CustomerID, Segment, 
                               RFM = paste(RecencyScore, FrequencyScore, MonetaryScore, sep = ""),
                               Receny = DaySinceLastTxn, Orders = NoOfTxn, 
                               TotalSpending = TotalSpending)]
  rfmSummary$Segment <- as.factor(rfmSummary$Segment)
  rfmSummary$RFM <- as.factor(rfmSummary$RFM)
  colnames(rfmSummary) <- c("Customer ID", "Segment", "RFM", "Recency (days)", "Orders", 
                            "Total Spending ($)")
  return(rfmSummary)
}

getAvgStatPerSegment <- function(rfmData, rfmScore, rfmSegment, 
                                 type = c("R", "F", "M"), bySex = TRUE) {
  # This function get the average statistics per segment.
  rfmResults <- getRFMResults(rfmData, rfmScore, rfmSegment)
  if (!("Gender" %in% colnames(rfmResults))) {
    bySex <- FALSE
  }
  selectedType <- switch (type,
                          "R" = "DaySinceLastTxn",
                          "F" = "NoOfTxn",
                          "M" = "TotalSpending")
  selectedName <- switch (type,
                          "R" = "Average Recency (days)",
                          "F" = "Average Number of Orders",
                          "M" = "Average Spending")
  
  if(bySex) {
    avgStatPerSegment <- rfmResults[, .(avg = mean(get(selectedType))), by = .(Segment, Gender)]
    colnames(avgStatPerSegment) <- c("Segment", "Gender", selectedName)
  } else {
    avgStatPerSegment <- rfmResults[, .(avg = mean(get(selectedType))), by = Segment]
    colnames(avgStatPerSegment) <- c("Segment", selectedName)
  }
  return(avgStatPerSegment)
}

getStatPerCustomer <- function(rfmData, type = c("F", "M")) {
  statPerMember <- if(type == "F") {
    stat <- rfmData[, .(CustomerID, NoOfTxn)]
    stat <- as.data.table(table(stat$NoOfTxn))
    stat$V1 <- as.numeric(stat$V1)
    colnames(stat) <- c("Number of Orders", "Number of Customers")
    stat
  } else {
    stat <- rfmData[, .(CustomerID, TotalSpending)]
    stat <- as.data.table(table(stat$TotalSpending))
    stat$V1 <- as.numeric(stat$V1)
    colnames(stat) <- c("Total Spending ($)", "Number of Customers")
    stat
  }
  return(statPerMember)
}

noOfCustomerPerSegmentDataTable <- function(dt) {
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
  return(
    DT::datatable(dt, class = 'compact', rownames= FALSE, 
                  container = sketch, # For column sum
                  options = list(lengthChange = FALSE, 
                                 pageLength = length(RFM_SEGMENT), # From findRFM.R
                                 searching = FALSE, dom = 't', 
                                 ordering = TRUE,
                                 footerCallback = JS(colTotalFooterJsCode)) # Column sum JS
    ) %>% formatPercentage("Percentage", 2))
}

plotPiePerSegment <- function(dataset, y, group, interactive = TRUE) {
  if (interactive) {
    if ("Percentage" %in% colnames(dataset)) {
      dataset$Percentage <- NULL
    }
    return(plotPieInteractive(dataset, y, group))
  } else {
    return(plotPieStatic(dataset, y, group))
  }
}

plotBarPerSegment <- function(dataset, x, y, group = NULL, 
                              y.labels.angle = 0, interactive = TRUE) {
  if (interactive) {
    return(plotBarPerGroupInteractive(dataset, x, y, group))
  } else {
    return(plotBarPerGroupStatic(dataset, x, y, group, y.labels.angle))
  }
}

plotNoOfCustomersPerSegment <- function(rfmData, rfmSegment, bySex = TRUE, 
                                        interactive = TRUE) {
  # This function plot the number of customers per segement in a bar chart (grouped by sex)
  noOfCustomerPerSegment <- getNoOfCustomersPerSegment(rfmData, rfmSegment, bySex)
  if (!("Gender" %in% colnames(noOfCustomerPerSegment))) {
    bySex <- FALSE
  }
  if (bySex) {
    g <- plotBarPerSegment(noOfCustomerPerSegment, x = colnames(noOfCustomerPerSegment)[1],
                           y = colnames(noOfCustomerPerSegment)[3],
                           group = colnames(noOfCustomerPerSegment)[2])
  } else {
    g <- plotPiePerSegment(noOfCustomerPerSegment, group = colnames(noOfCustomerPerSegment)[1],
                           y = colnames(noOfCustomerPerSegment)[2], interactive)
  }
  return(g)
}

plotAvgStatPerSegment <- function(rfmData, rfmScore, rfmSegment, 
                                  type = c("R", "F", "M"), bySex = TRUE) {
  avgStatPerSegment <- getAvgStatPerSegment(rfmData, rfmScore, rfmSegment, type, bySex)
  if (!("Gender" %in% colnames(avgStatPerSegment))) {
    bySex <- FALSE
  }
  if (bySex) {
    g <- plotBarPerSegment(avgStatPerSegment, x = colnames(avgStatPerSegment)[1],
                           y = colnames(avgStatPerSegment)[3],
                           group = colnames(avgStatPerSegment)[2])
  } else {
    g <- plotBarPerSegment(avgStatPerSegment, x = colnames(avgStatPerSegment)[1],
                           y = colnames(avgStatPerSegment)[2])
  }
  return(g)
}

plotBarPerCustomer <- function(dataset, x, y, group = NULL, x.labels.angle = 0,
                               interactive = TRUE) {
  if (interactive) {
    return(plotColumnPerGroupInteractive(dataset, x, y, group))
  } else {
    return(plotColumnPerGroupStatic(dataset, x, y, group, x.labels.angle))
  }
}

binning <- function(data, type = c("int", "num"), n.bins = 10) {
  if(type == "int") {
    # writeLines("Number of Customer by F")
    statsLastBin <- sum(data[get(colnames(data)[1]) >= n.bins, 
                                      get(colnames(data)[2])])
    if (statsLastBin > 0) {
      data <- data[get(colnames(data)[1]) < n.bins]
      data[, colnames(data)[1] := as.character(
        get(colnames(data)[1]))]
      data <- rbindlist(list(data, list(
        paste(n.bins, "+", sep = ""), statsLastBin)))
    }
    x.labels.angle <- 0
  } else {
    breaks <- unique(round(unname(quantile(data[,get(colnames(data)[1])], 
                                           probs = (seq(0, 1, 1/n.bins)))), digits = -3))
    # writeLines("Number of Customer by M")
    # print(breaks)
    # writeLines("")
    inInterval <- findInterval(data[,get(colnames(data)[1])], breaks, 
                               all.inside = TRUE)
    intervalLabels <- character()
    for (i in 2:length(breaks)) {
      labelTemp <- if (i != (length(breaks))) {
        paste(breaks[(i-1):i], collapse = " - ")
      } else {
        paste(">", breaks[i-1], sep = "")
      }
      intervalLabels <- c(intervalLabels, labelTemp)
    }
    # print(intervalLabels)
    # writeLines("")
    dataTemp <- cbind(data, inInterval)
    dataAgg <- dataTemp[,.(sum(get(colnames(dataTemp)[2]))), 
                                          by = inInterval]
    dataAgg <- cbind(intervalLabels, dataAgg[,-1])
    colnames(dataAgg) <- colnames(data)
    data <- dataAgg
    x.labels.angle <- 45
  }
  statLevels <- unique(data[, get(colnames(data)[1])])
  # print(statLevels)
  # writeLines("")
  data[, colnames(data)[1] := factor(
    get(colnames(data)[1]), levels = statLevels)]
  return(list(data = data, x.labels.angle = x.labels.angle))
}

plotStatPerCustomer <- function(rfmData, type = c("F", "M"), n.bins = 10,
                                interactive = TRUE) {
  statPerMember <- getStatPerCustomer(rfmData, type)
  if (type == "F") {
    statPerMemberList <- binning(data = statPerMember, type = "int", n.bins = n.bins)
  } else {
    statPerMemberList <- binning(data = statPerMember, type = "num", n.bins = n.bins)
  }
  statPerMember <- statPerMemberList$data
  x.labels.angle <- statPerMemberList$x.labels.angle
  g <- plotBarPerCustomer(statPerMember, colnames(statPerMember)[1],
                          colnames(statPerMember)[2], 
                          x.labels.angle = x.labels.angle, 
                          interactive = interactive)
  return(g)
}