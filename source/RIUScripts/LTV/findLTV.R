library(BTYD)
library(BTYDplus)
library(dplyr)
library(parallel)
library(scales)
source("../LTV/BTYDmodels.R")
source("../RFM/findRFM.R")
source("../Visualizer/graphics.R")

computeDiscountRate <- function(d, predictionPeriod) {
  avg.time <- (predictionPeriod$begin + predictionPeriod$end) / 2
  return((1/(1 + d)^(predictionPeriod$period / 365 * avg.time)))
}

computeDERT.getMaxPeriod <- function(x, t.x, T.cal) {
  max.length <- max(length(x), length(t.x), length(T.cal))
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  if (max.length%%length(t.x)) 
    warning("Maximum vector length not a multiple of the length of t.x")
  if (max.length%%length(T.cal)) 
    warning("Maximum vector length not a multiple of the length of T.cal")
  
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x)) 
    stop("t.x must be numeric and may not contain negative numbers.")
  if (any(T.cal < 0) || !is.numeric(T.cal)) 
    stop("T.cal must be numeric and may not contain negative numbers.")
  return(max.length)
}

computeDERT.threshold <- function(DERT, thres = 1e-4) {
  # To remove some negligible DERT value
  DERT[DERT < thres]$DERT <- 0
  return(DERT)
}

computeDERT <- function(cbs, method, model.params, d,
                        units = "week", nperiod = NULL){
  if (!tolower(method) %in% c("pnbd", "bgnbd", "mbgnbd", "pnbd.hb", "pggg")){
    stop("method is not valid")
  }
  predictionPeriod <- getPredictionPeriod(units, nperiod)
  discountRate <- computeDiscountRate(d, predictionPeriod)
  if (tolower(method) %in% c("pnbd", "bgnbd", "mbgnbd")) {
    # TODO: chunk in parallel
    DERT <- cbs[, .(cust, x, t.x, T.cal)]
    DERT <- DERT[, .(DERT = computeDERT.mle(method, model.params, x, t.x,
                                            T.cal, predictionPeriod, discountRate)), by = .(cust)]
  } else {
    DERT <- computeDERT.mcmc(method, cbs, model.params, predictionPeriod, discountRate)
  }
  return(computeDERT.threshold(DERT))
}

computeDERT.mle <- function(method, model.params, x, t.x, T.cal, 
                            predictionPeriod, discountRate){
  if (!tolower(method) %in% c("pnbd", "bgnbd", "mbgnbd")){
    stop("method is not valid")
  }
  max.length <- computeDERT.getMaxPeriod(x, t.x, T.cal)
  x <- rep(x, length.out = max.length)
  t.x <- rep(t.x, length.out = max.length)
  T.cal <- rep(T.cal, length.out = max.length)
  # TODO: Possible to apply the diff-like function to avoid repeated computations
  delta.txn <- futureTxnLevel.mle(model.params, x, t.x, T.cal, 
                                  predictionPeriod$end, method)
  delta.txn <- delta.txn - futureTxnLevel.mle(model.params, x, t.x, T.cal, 
                                              predictionPeriod$begin, method)
  discounted.delta.txn <- discountRate * delta.txn
  dert <- sum(discounted.delta.txn)
  return(dert)
}

computeDERT.mcmc <- function(method, cbs, draws, predictionPeriod, discountRate){
  if (!tolower(method) %in% c("pnbd.hb","pggg")){
    stop("method is not valid")
  }
  # TODO: Iterate throught the time idx
  cust_sel <- names(draws$level_1) # Select customers that have draws
  delta.txn <- futureTxnLevel.mcmc(cbs[cust %in% cust_sel], draws, predictionPeriod$end, method)
  delta.txn <- delta.txn - futureTxnLevel.mcmc(cbs[cust %in% cust_sel], draws, predictionPeriod$begin, method)
  discounted.delta.txn <- discountRate * delta.txn
  dert <- colSums(discounted.delta.txn)
  dert <- data.table(cust = cust_sel, DERT = dert)
  return(dert)
}

findLTV <- function(cbs, model.params, monetaryValue.params = NULL,
                    method = c("pnbd", "bgnbd", "mbgnbd", "pnbd.hb", "pggg"), 
                    d = 0.3, units = "week", nperiod = 52, margin = 0.3) {
  method <- match.arg(method)
  # compute the expected monetary value per transation for each customer
  expected.MonetaryValue <- expectation.monetaryValue.per.txn(cbs, monetaryValue.params)
  DERT <- computeDERT(cbs, method, model.params, d, units, nperiod)
  
  setkey(expected.MonetaryValue, cust)
  setkey(DERT, cust)
  ltv <- DERT[expected.MonetaryValue, nomatch = 0]
  ltv <- ltv[, .(lifetimeValue = DERT * expected.avg.txn.value * margin), by = .(cust)]
  setnames(ltv, c("cust"), c("CustomerID"))
  return(ltv)
}

findPC <- function(ltv, id = colnames(ltv)[1], 
                   attr.names = colnames(ltv)[2:3]){
  # compute the percentile of Palive and CLT and the score of the Palive and CLT
  PCindividualScore <- function(percentile) {
    return(findInterval(percentile, seq(0, 1, by = 0.25), left.open = TRUE))
  }
  PC.table <- ltv[, c("Palive.Percentile", "LTV.Percentile") := list(
    percent_rank(get(attr.names[2])), percent_rank(get(attr.names[1])))]
  PC.table <- PC.table[, c("Palive.Score", "LTV.Score") :=  list(
    PCindividualScore(Palive.Percentile), PCindividualScore(LTV.Percentile)), 
    by = id]
  return(PC.table)
}

plotLTVPerSegment <- function(ltv, rfmSegment, interactive = TRUE) {
  ltv <- copy(ltv)
  setkeyv(ltv, "CustomerID")
  setkeyv(rfmSegment, "CustomerID")
  ltv <- merge(ltv, rfmSegment, by = "CustomerID", all = TRUE)
  ltv[is.na(ltv$Segment)]$Segment <- "Lost Cheap Customers"
  df <- ltv[, .(avg = mean(lifetimeValue)), by = .(Segment)]
  overall <- ltv[, .(Segment = "Overall", avg = mean(lifetimeValue))]
  df <- rbind(df, overall)
  
  segementOrder <- c(RFM_SEGMENT, "Overall")  # RFM_SEGMENT defined in 
                                              # ../RFM/findRFM.R
  df$Segment <- factor(df$Segment, segementOrder, ordered = TRUE)
  df <- melt(df, id.vars = c("Segment"))
  setorderv(df, "Segment")
  setnames(df, "value", "Average Customer Lifetime Value ($)")
  
  if (interactive) {
    g <- plotBarPerGroupInteractive(df, colnames(df)[1], colnames(df)[3], 
                                    logScale = TRUE)
  } else {
    g <- plotBarPerGroupStatic(df, colnames(df)[1], colnames(df)[3]) + 
      theme_minimal() + theme(legend.title = element_blank()) +
      scale_y_continuous(labels = comma)
  }
  return(g)
}