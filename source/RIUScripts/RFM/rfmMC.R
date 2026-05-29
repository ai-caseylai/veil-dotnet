library(lubridate)
library(data.table)
library(dplyr)
library(qgraph)
library(visNetwork)
library(parallel)
source("../RFM/findRFM.R")

TIME_UNIT_AVAILABLE <- c("year", "week", "month", "day")

txn2rfmSegmentList <- function(txnData, rfmPeriod = c(1, "year"), transitionPeriod = c(1, "month"), sumTotal = FALSE) {
  if (!(rfmPeriod[2] %in% TIME_UNIT_AVAILABLE && transitionPeriod[2] %in% TIME_UNIT_AVAILABLE)) {
    stop("Wrong time unit input.")
  }
  if (is.POSIXct(txnData$Timestamp)) {
    txnData$Timestamp <- as.POSIXct(txnData$Timestamp)
  }
  rfmPeriodUnit <- switch (rfmPeriod[2],
                           "year" = years,
                           "month" = months,
                           "week" = weeks,
                           "day" = days
  )
  transPeriodUnit <- switch (transitionPeriod[2],
                             "year" = years,
                             "month" = months,
                             "week" = weeks,
                             "day" = days
  )
  beginDate <- as.Date(min(txnData$Timestamp)) + rfmPeriodUnit(as.integer(rfmPeriod[1]))
  endDate <- as.Date(max(txnData$Timestamp))
  nPeriods <- interval(beginDate, endDate) %/% transPeriodUnit(as.integer(transitionPeriod[1]))
  
  iterates <- seq(-nPeriods, 0, as.integer(transitionPeriod[1]))
  # Calculate the number of cores
  no_cores <- min(max(1, detectCores() - 1), length(iterates))
  # Initiate cluster
  cl <- makeCluster(no_cores)
  clusterEvalQ(cl, {
    library(lubridate)
    library(data.table)
    library(dplyr)
    source("../RFM/findRFM.R")
  })
  clusterExport(cl, ls(), envir=environment())

  rfmSegmentList <- parLapply(cl, iterates, function(periodBegin) {
    endRFMDate <- endDate + transPeriodUnit(periodBegin * as.integer(transitionPeriod[1]))
    beginRFMDate <- endRFMDate - rfmPeriodUnit(as.integer(rfmPeriod[1]))
    return(rfmSegmentation(rfmCompute(getRFMData(
      txnData[Timestamp >=  beginRFMDate & Timestamp <= endRFMDate], sumTotal))))
  })
  stopCluster(cl)
  
  # rfmSegmentList <- lapply(iterates, function(periodBegin) {
  #   endRFMDate <- endDate + transPeriodUnit(periodBegin * as.integer(transitionPeriod[1]))
  #   beginRFMDate <- endRFMDate - rfmPeriodUnit(as.integer(rfmPeriod[1]))
  #   return(rfmSegmentation(rfmCompute(getRFMData(
  #     txnData[Timestamp >=  beginRFMDate & Timestamp <= endRFMDate], sumTotal))))
  # })
  
  return(rfmSegmentList)
}

rfmSegment2mcSeq <- function(rfmSegmentList) {
  # rfmSegmentList is a list of data.table with columns: CustomerID, Segment
  if (!is.list(rfmSegmentList)) {
    stop("The argument: rfmSegmentList must be a list.")
  }
  mcSeq <- Reduce(
    (function() {
      counter = 0
      function(x, y) {
        counter <<- counter + 1
        if (counter == 1) {
          setnames(x, colnames(x)[-1], '0')
        }
        d <- merge(x, y, all = TRUE, by = "CustomerID")
        setnames(d, c(head(names(d), -1), paste0(counter, sep = "")))
        # Set the disappeared customer to be lost
        d[is.na(get(paste0(counter, sep = ""))) & !is.na(get(paste0(counter-1, sep = ""))),
          (paste0(counter, sep = "")) := "Lost Cheap Customers"]
      }
    })(), rfmSegmentList)
  mcSeq <- melt(mcSeq, id.vars = "CustomerID", variable.name = "Time", variable.factor = FALSE, 
                value.name = "State", na.rm = FALSE)
  setnames(mcSeq, "CustomerID", "ID")
  mcSeq$Time <- as.integer(mcSeq$Time)
  mcSeq$State <- as.factor(mcSeq$State)
  return(mcSeq)
}

mcSeq2Counts <- function(mcSeq, stateSpace = NULL) {
  # Convert the mcSeq to transition probabilities
  if (is.null(stateSpace)) {
    stateSpace <- unique(mcSeq$State)
    stateSpace <- stateSpace[!is.na(stateSpace)]
  } else {
    if (!all(stateSpace %in% unique(mcSeq$State))) {
      stop("Argument stateSpace not match with that in mcSeq.")
    } else {
      mcSeq$State <- factor(mcSeq$State, RFM_SEGMENT)
    }
  }
  nPeriods <- max(mcSeq$Time)
  iterates <- seq.int(1, nPeriods)
  mcCounts <- lapply(iterates, function(timeIn) {
    as.data.frame.matrix(table(mcSeq[Time==(timeIn-1), State], mcSeq[Time==(timeIn), State]))
  })
  return(mcCounts)
}

mcCounts2Prob <- function(mcCounts, stationary = TRUE, stateSpace = NULL) {
  normalize <- function(x) x / rowSums(x)
  if (stationary) {
    mcCounts <- Reduce("+", mcCounts)
    transProb <- normalize(mcCounts)
    return(as.matrix(transProb))
  } else {
    transProb <- lapply(mcCounts, function(count) {
      return(as.matrix(normalize(count)))
    })
    return(transProb)
  }
}

lrtest <- function(mcCounts, stateSpace = NULL) {
  transProbStationary <- mcCounts2Prob(mcCounts, TRUE, stateSpace)
  transProbNonStationary <- mcCounts2Prob(mcCounts, FALSE, stateSpace)
  logLambda <- lapply(seq.int(1, length(mcCounts)), function(i) {
    output <- as.matrix(mcCounts[[i]] * log(transProbStationary / transProbNonStationary[[i]]))
    output[transProbNonStationary[[i]]==0] <- 0
    return(output)
  })
  return(-2 * sum(Reduce("+", logLambda)))
}

plotTransitionStatic <- function(transProb, ...) {
  nodeNames <- colnames(transProb)
  rownames(transProb) <- colnames(transProb) <- seq.int(nrow(transProb))
  
  g <- qgraph(transProb, directed = TRUE, fade = FALSE, edge.labels = TRUE, 
              nodeNames = nodeNames, ...)
  return(g)
}

plotTransitionInteractive <- function(transProb, threshold, height = 640, 
                                      width = "100%") {
  prob2width <- function(p, a = 1, b = 4) {
    # probability to width function
    return(((sin(p*pi/2))^2)*(b-a) + a)
  }
  
  getNodes <- function(nodeIdMap) {
    nodeId <- as.integer(nodeIdMap)
    nodeTitle <- as.character(nodeIdMap)
    nodeDf <- data.table(id = nodeId, 
                         label = as.character(nodeId),
                         title = nodeTitle,
                         value = 20,
                         font.size = 20,
                         shape = "circle")
    nodeGroup <- list(Loyal = list(index = (1:3), shape = "ellipse", 
                                   group = "Loyal"), 
                      Active = list(index = (4:5), shape = "square", 
                                    group = "Active"),
                      Losing = list(index = (6:8), shape = "circle", 
                                    group = "Losing"), 
                      Lost = list(index = (9:11), shape = "triangle", 
                                  group = "Lost"))
    lapply(nodeGroup, function(x) {
      nodeDf[title %in% RFM_SEGMENT[x$index], group := x$group]
    })
    return(nodeDf)
  }
  
  transProb2Graph <- function(probs = transProb, thres = threshold) {
    transProbDf <- as.data.table(as.data.frame(probs), keep.rownames = "From")
    transProbDf <- melt(transProbDf, id.vars = "From", variable.name = "To", value.name = "Prob")
    transProbDf <- transProbDf[Prob > threshold]
    nodeIdMap <- factor(unique(transProbDf$To))
    transProbDf$To <- factor(transProbDf$To, nodeIdMap)
    transProbDf$From <- factor(transProbDf$From, nodeIdMap)
    
    edgeDf <- transProbDf[, .(from = as.integer(From), 
                              to = as.integer(To), 
                              width = prob2width(Prob, 1, 4), 
                              title = paste(as.character(From), " -> ", 
                                            as.character(To), 
                                            "<br>Probability: ", 
                                            round(Prob, 4) * 100, "%<br>", 
                                            sep = ""), 
                              label = round(Prob, 2),
                              color = "black",
                              font.color = "black",
                              font.size = 18,
                              arrows = "to", smooth = TRUE)]
    
    edgeDf <- edgeDf[from == to, color := "blue"]
    edgeDf <- edgeDf[from == to, font.color := "blue"]
    
    edgeDf <- edgeDf[from == to & 
                       from == which(RFM_SEGMENT == "Lost Cheap Customers"), 
                     color := "purple"]
    edgeDf <- edgeDf[from == to & 
                       from == which(RFM_SEGMENT == "Lost Cheap Customers"), 
                     font.color := "purple"]
    
    edgeDf <- edgeDf[from > to, color := "red"]
    edgeDf <- edgeDf[from > to, font.color := "red"]
    return(list(nodes = getNodes(nodeIdMap), edges = edgeDf))
  }
  
  graph <- transProb2Graph()
  g <- visNetwork(graph$nodes, graph$edges, height = height, width = width)
  g <- visLegend(g, useGroups = TRUE, position = "right", zoom = FALSE)
  return(g)
}

plotRFMTransition <- function(transProb, threshold = 0.05, legend = FALSE, 
                              interactive = TRUE) {
  if(is.list(transProb)) {
    # Plot only the latest transition probabilities
    transProb <- transProb[[length(transProb)]]
  }
  if (interactive) {
    return(plotTransitionInteractive(transProb, threshold))
  } else {
    nodeNames <- colnames(transProb)
    
    edgeColour <- matrix(0, nrow(transProb), ncol(transProb), dimnames = dimnames(transProb))
    diag(edgeColour) <- "blue"
    edgeColour[upper.tri(edgeColour)] <- "darkgreen"
    edgeColour[lower.tri(edgeColour)] <- "red"
    lostCheapCustIdx <- which(nodeNames == "Lost Cheap Customers")
    edgeColour[lostCheapCustIdx, lostCheapCustIdx] <- "purple"
    
    nodeGroup <- list(Loyal = (1:3), Active = (4:5), Losing = (6:8), Lost = (9:11))
    nodeShape <- c(rep("ellipse", 3), rep("square",2), rep("circle",3), rep("triangle",3))
    g <- plotTransitionStatic(transProb, minimum = threshold, 
                        groups = nodeGroup, colFactor = 0.1, 
                        shape = nodeShape, node.resolution = 800, label.cex = 1.5,
                        borders = FALSE, vTrans = 120, node.width = 1, node.height = 1,
                        edge.color = edgeColour, edge.label.cex = 1.25, esize = 2,
                        edge.labels = TRUE, edge.width= 2, edge.label.bg = "transparent",
                        edge.label.position = 0.35,
                        GLratio = 2.5, legend.cex = 0.5, rescale = TRUE, aspect = TRUE,
                        legend.mode = "names", bg = "transparent", legend = legend)
    return(g)
  }
}

mcSeq2Prop <- function(mcSeq, period = length(mcSeq)) {
  selectedSeq <- mcSeq[[period]]
  prop <- table(selectedSeq) / sum(table(selectedSeq))
  prop <- prop[RFM_SEGMENT]
  
  prop.matrix <- matrix(prop)
  row.names(prop.matrix) <- RFM_SEGMENT
  colnames(prop.matrix) <- c("proportion")
  prop.matrix <- t(prop.matrix)
  return(prop.matrix)
}

mcSeq2SegmentMovement <- function(mcSeq) {
  segmentMovement <- mcSeq[, .(Actual = .N), by = .(Time, State)]
  segmentMovement <- segmentMovement[!is.na(State)]
  setnames(segmentMovement, "State", "Segment")
  return(segmentMovement)
}

predictSegmentMovement <- function(initProp, transProb, nCustomer, 
                                   nperiod = 6) {
  predictedSegment <- initProp
  for (i in seq(nperiod)) {
    initProp <- initProp %*% transProb
    predictedSegment <- rbind(predictedSegment, initProp)
  }
  predictedSegment <- predictedSegment * nCustomer
  row.names(predictedSegment) <- c(0:nperiod)
  predictedSegment <- melt(as.data.table(predictedSegment, keep.rownames = "Time"), id.vars = "Time", variable.name = "Segment", value.name = "Predicted")
  colnames(predictedSegment) <- c("Time", "Segment", "Predicted")
  return(predictedSegment)
}

plotSegmentMovement <- function(mcSeq, transProb, begin = NULL, nperiod = 6, 
                                unit = "month") {
  initProp <- mcSeq2Prop(mcSeq)
  nCustomer <- mcSeq[Time == max(Time), .N]
  predictedSegmentMovement <- predictSegmentMovement(initProp, transProb, 
                                                     nCustomer, nperiod)
  
  yticks <- c(2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536)
  g <- ggplot(data = predictedSegmentMovement, 
              aes(x = Time, y = Predicted, group = Segment))
  g <- g + geom_line(aes(color = Segment), size = 0.9)
  g <- g + xlab("Period") + ylab("Predicted Number of Customers")
  g <- g + scale_y_continuous(trans='log2', breaks = yticks)
  g <- g + theme_bw() + theme(axis.text.y = element_text(size = 15), 
                              axis.title = element_text(size = 15),
                              axis.text.x = element_text(size = 15),
                              legend.title = element_blank(),
                              legend.text = element_text(size = 15))
  return(g)
}