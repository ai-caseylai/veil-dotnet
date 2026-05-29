library(data.table)
source("../RFM/findRFM.R")

MonetaryValue.EstimateParameters <- function(cal.cbs, par.start = c(1,1,1),
                                             max.param.value = 100000){
  # this function is to find the Gamma/Gamma submodel parameters (p,q,gamma) to maximize the likelihood function
  # Gamma/Gamma Submodel:
  #                     The dollar value of a customer's given transaction is distributed gamma with 
  #                     shape parameter p and scale parameter nu
  #
  #                     heterogeneity in scale parameter nu across customers
  #                     follows a gamma distribution with shape parameter q and scale parameter gamma
  #
  # detail: RFM and CLV: Using Iso-value Curves for Customers Base Analysis    Section 2.1
  #
  # arguments:  
  #             cal.cbs: the dataset contain the cust, m.x (customer's average observed transaction value), x (number of txns)
  #             par.start: set the initial value for the parameters value
  #             max.param.value: set the maximun value for parameters value
  
  # helper function to be optimized
  MonetaryValue.eLL <- function(params, cal.cbs, max.param.value){
    params <- exp(params)
    params[params > max.param.value] <- max.param.value
    return(-1 * MonetaryValue.cbs.LL(params, cal.cbs))
  }
  logparams <- log(par.start)
  results <- optim(logparams, MonetaryValue.eLL, cal.cbs = cal.cbs, max.param.value = max.param.value,
                   method = "L-BFGS-B")
  estimated.params = exp(results$par)
  estimated.params[estimated.params > max.param.value] <- max.param.value
  return(estimated.params)
}


MonetaryValue.cbs.LL <- function(params, cal.cbs){
  # this function is to compute the sum of customers' likelihood value
  # arguments: params: the parameter value for the Gamma/Gamma Submodel for Monetary Value
  #            cal.cbs: the dataset contain the cust, m.x (customer's average observed transaction value), x (number of txns)
  tryCatch(m.x <- cal.cbs[, "m.x"], error = function(e) stop("Error in MonetaryValue.cbs.LL: cal.cbs must have a average_txn_value column labelled \"m.x\""))
  tryCatch(x <- cal.cbs[, "x"], error = function(e) stop("Error in MonetaryValue.cbs.LL: cal.cbs must have a frequency column labelled \"x\""))
  if ("custs" %in% colnames(cal.cbs)) {
    custs <- cal.cbs[, "custs"]
  } else {
    custs <- rep(1, length(x))
  }
  return(sum(custs * MonetaryValue.LL(params, m.x, x)))
}


MonetaryValue.LL <- function(params, m.x, x){
  # this function is to compute the likelihood value of Gamma/Gamma submodel for Monetory Value
  # 
  # arguments: params: paramester of the Gamma/Gamma Submodel
  #            m.x:    customer's average observed transaction value
  #            x:      number of transactions
  #
  # detail: RFM and CLV: Using Iso-value Curves for Customers Base Analysis    Section 2.1
  max.length <- max(length(m.x), length(x))
  
  if (max.length%%length(m.x)) 
    warning("Maximum vector length not a multiple of the length of m.x")
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  
  if (any(m.x < 0) || !is.numeric(m.x)) 
    stop("m.x must be numeric and may not contain negative numbers.")
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  
  m.x <- rep(m.x, length.out = max.length)
  x <- rep(x, length.out = max.length)
  
  p <- params[1]
  q <- params[2]
  gamma <- params[3]
  
  part1 <- lgamma(p * x + q) - lgamma(p * x) - lgamma(q)
  part2 <- q * log(gamma) + (p * x - 1) * log(m.x) + p * x * log(x) - (p * x + q) * log(gamma + m.x * x)
  return(part1 + part2)
}

MonetaryValue.ConditionalExpectedAvgTxnValue <- function(params, m.x, x){
  # this function is to computed the average transaction value for a customer with an average spend of m.x across x transactions
  max.length <- max(length(m.x), length(x))
  
  if (max.length%%length(m.x)) 
    warning("Maximum vector length not a multiple of the length of m.x")
  if (max.length%%length(x)) 
    warning("Maximum vector length not a multiple of the length of x")
  
  
  if (any(m.x < 0) || !is.numeric(m.x)) 
    stop("m.x must be numeric and may not contain negative numbers.")
  if (any(x < 0) || !is.numeric(x)) 
    stop("x must be numeric and may not contain negative numbers.")
  
  m.x <- rep(m.x, length.out = max.length)
  x <- rep(x, length.out = max.length)
  
  p <- params[1]
  q <- params[2]
  gamma <- params[3]
  
  part1 <- (gamma * p) / (p * x + q - 1)
  part2 <- (m.x * p * x) / (p * x + q - 1)
  return(part1 + part2)
}

fit.MonetaryValue <- function(elog) {
  # need the transactions data contain columns: 
  #                                             cust: customer ID
  #                                             date: purchase date
  #                                             sales: monetary value per transation
  elog <- elog[,.(m.x = mean(sales), x = (.N)), by = .(cust)]
  elog <- elog[m.x >0] # To remove customer with zero average spending
  # estimate the parameter of Gamma/Gamma Submodel for Monetary Value
  params.MonetaryValue <- MonetaryValue.EstimateParameters(as.data.frame(elog))
  return(params.MonetaryValue)
}

expectation.monetaryValue.per.txn <- function(elog, params = NULL){
  # need the transactions data contain columns: 
  #                                             cust: customer ID
  #                                             date: purchase date
  #                                             sales: monetary value per transation
  elog <- elog[,.(m.x = mean(sales), x = (.N)), by = .(cust)]
  
  if(is.null(params)) {
    # estimate the parameter of Gamma/Gamma Submodel for Monetary Value
    params <- MonetaryValue.EstimateParameters(as.data.frame(elog))
  }
  # compute the expected monetary value per transation for each customer
  result <- elog[,expected.avg.txn.value := 
                  MonetaryValue.ConditionalExpectedAvgTxnValue(params,m.x,x)]
  result <- result[,.(cust,m.x,expected.avg.txn.value)]
  return(result)
}

plotAvgSpendingPerSegment <- function(avgSpending, rfmSegment) {
  # avgSpending is dt from expectation.monetaryValue.per.txn
  # rfmSegment is dt from rfmSegmentation
  avgSpending <- copy(avgSpending)
  setkeyv(avgSpending, "cust")
  setnames(avgSpending, "cust", "CustomerID")
  
  setkeyv(rfmSegment, "CustomerID")
  avgSpending <- merge(avgSpending, rfmSegment, by = "CustomerID")
  
  df <- avgSpending[, .(Actual = mean(m.x), Predicted = mean(expected.avg.txn.value)), by = .(Segment)]
  overall <- avgSpending[, .(Segment = "Overall", Actual = mean(m.x), Predicted = mean(expected.avg.txn.value))]
  df <- rbind(df, overall)
  
  segementOrder <- c("Best Customers", "Loyal Customers", "Potential Loyalist", "Low-spending Active Loyal Customers",
                     "High-spending New Customers", "Almost Lost Customers", "Churned Best Customers", 
                     "Customers Needing Attention", "About to Sleep Customers", "Hibernating Customers",
                     "Lost Cheap Customers", "Overall")
  df$Segment <- factor(df$Segment, segementOrder)
  df <- melt(df, id.vars = c("Segment"))
  setnames(df, "value", "Average Spending per Transaction")
  
  g <- plotBarPerSegment(df, colnames(df)[1], colnames(df)[3], colnames(df)[2]) +
    theme_minimal() + theme(legend.title = element_blank()) +
    scale_y_continuous(labels = comma)
  return(g)
}

m.x.marginal <- function(params, x, m.x){
  # marginal distribution of m.x
  # reference:
  #    RFM and CLV:Using Iso-value Curves for customers Base Analysis
  p <- params[1]
  q <- params[2]
  gamma <- params[3]
  lmarginal <- lgamma(p*x+q) - lgamma(p*x) - lgamma(q) + 
    q*log(gamma) +(p*x-1)*log(m.x) + p*x*log(x) - 
    (p*x + q)*log(gamma + x*m.x)
  return(exp(lmarginal))
}