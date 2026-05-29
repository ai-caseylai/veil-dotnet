library(BTYD)
library(BTYDplus)
library(coda)
library(data.table)
library(ggplot2)
library(lubridate)
library(parallel)
source("../Utils/master.R")

getCBS <- function(elog, p = 0.2, T.cal = NULL, units = "weeks") {
  if(is.null(T.cal)) {
    T.cal <- getHoldoutDate(elog, p)
  }
  return(elog2cbs(elog, units = units, T.cal = T.cal, T.tot = max(elog$date)))
}

getSelectedCBS <- function(elog, p = 0.2, T.cal = NULL, units = "weeks") {
  # convert Transation to CBS (customer-by-sufficient-statistic) data.table
  # split the data into calibration period and holdout period
  if(is.null(T.cal)){
    T.cal <- getHoldoutDate(elog, p)
  }
  cbsData <- elog2cbs(elog, units = units, T.cal = T.cal, T.tot = max(elog$date))
  
  # select the customers who in the acquired period for model training
  cbsData <- cbsData[first <= getAcquiredDate(elog, p),]
  
  # remove the outlier which will get the infinite log-likelihood value using the approx. param
  approx.param <- c(0.5290,10.4009,0.1644,1.3681)
  cbsData <- removeOutliers(cbsData, approx.param)
  return(cbsData)
}

removeOutliers <- function(cbs, approx.param) {
  # remove the outlier which will generate the infinite log-likelihood value
  cbs <- cbs[,LL := pnbd.LL(params = approx.param,x = x,t.x = t.x,T.cal = T.cal)]
  cbs <- cbs[LL != -Inf,]
  cbs <- cbs[,LL := NULL]
  return(cbs)
}

getCalElog <- function(elog, cbs, holdoutDate) {
  # Get the elog in Calibration period for model training
  return(elog[cust %in% cbs$cust & date <= holdoutDate])
}

mae <- function(act, est){
  # Compute the MAE
  stopifnot(length(act) == length(est))
  absError <- abs(act-est)
  return(sum(absError, na.rm = TRUE) / length(absError[!is.na(absError)]))
}

chisq.stat <- function(freq, modelName, censor, nparam) {
  colnames(freq) <- c("Actual", modelName)
  writeLines(paste(modelName, ": Chi-squared statistics", sep = ""))
  chisq.pnbd <- as.numeric(chisq.test(freq)[1])
  print(chisq.pnbd)
  writeLines("P-value:")
  print(pchisq(chisq.pnbd, df = (censor + 1 - nparam), lower.tail = FALSE))
}

fit.BTYD <- function(cbs, elog, method = c("pnbd", "bgnbd", "mbgnbd", "pnbd.hb", "pggg"),
                     censor = 10, graph = FALSE, params.save = FALSE,
                     log_path = paste("Logs/", format(Sys.time(), "%Y%m%d"), sep = "")) {
  method <- match.arg(method)
  fit <- switch(method, 
                "pnbd" = fit.pnbd,
                "bgnbd" = fit.bgnbd,
                "mbgnbd" = fit.mbgnbd,
                "pnbd.hb" = fit.pnbdHB,
                "pggg" = fit.pggg)
  return(fit(cbs, elog, censor, graph, params.save, log_path))
}

fit.pnbd <- function(cbs, elog, censor = 10, graph = FALSE, params.save = FALSE,
                     log_path = paste("Logs/", format(Sys.time(), "%Y%m%d"), sep = "")) {
  if(graph) {
    if(!dir.exists(log_path)) {
      dir.create(log_path, FALSE, TRUE)
    }
    sink(paste(log_path, "/pnbd_train_", format(Sys.time(), "%Y%m%d_%H%M"), ".log", sep = ""))
  }
  # estimate Pareto/NBD parameters
  # pnbd.EstimateParameters is modified
  params.pnbd <- pnbd.EstimateParameters(as.data.frame(cbs))
  # report log-likelihood 
  LL.pnbd <- pnbd.cbs.LL(params = params.pnbd, cal.cbs = as.data.frame(cbs))
  
  # estimated and actual frequencies of repeat purchases in the calibration period
  if(graph) {
    if(!dir.exists(log_path)) {
      dir.create(log_path, FALSE, TRUE)
    }
    pdf(paste(log_path, "/pnbd_freq_in_cal_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  freq.pnbd <- pnbd.PlotFrequencyInCalibration(params = params.pnbd, cal.cbs = cbs, 
                                               censor = censor)
  if(graph) {
    dev.off()
  }
  
  freq.pnbd <- t(as.data.table(freq.pnbd))
  chisq.stat(freq.pnbd, "Pareto/NBD", censor, length(params.pnbd))
  
  # predict the excepted purchase quantity for each customers in the holdout period
  xstar.pnbd <- pnbd.ConditionalExpectedTransactions(params = params.pnbd,
                                                     T.star = cbs$T.star,x = cbs$x,
                                                     t.x = cbs$t.x, T.cal = cbs$T.cal)
  
  writeLines("Pareto/NBD: MAE")
  mae.pnbd <- mae(act = cbs$x.star, est = xstar.pnbd)
  print(mae.pnbd)
  print(summary(cbs$x.star - xstar.pnbd))
  
  # tracking the weekly transations
  if(graph) {
    pdf(paste(log_path, "/pnbd_no_of_txn_tracking_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  nil.pnbd <- pnbd.PlotTrackingInc(params = params.pnbd, T.cal = cbs$T.cal, 
                                   T.tot = max(cbs$T.cal + cbs$T.star),
                                   actual.inc.tracking.data = elog2inc(as.data.frame(elog)))
  if(graph) {
    dev.off()
    sink()
  }
  if(params.save) {
    saveRDS(params.pnbd, paste(log_path, "/pnbd.rds", sep = ""), compress = "xz")
  }
  return(params.pnbd)
}

fit.pnbdHB <- function(cbs, elog, censor = 10, graph = FALSE, params.save = FALSE, 
                       log_path = paste("Logs/", format(Sys.time(), "%Y%m%d"), sep = "")) {
  if(graph) {
    if(!dir.exists(log_path)) {
      dir.create(log_path, FALSE, TRUE)
    }
    sink(paste(log_path, "/pnbdHB_train_", format(Sys.time(), "%Y%m%d_%H%M"), ".log", sep = ""))
  }
  # generate parameters draws under Pareto/NBD(HB)
  pnbd.draws <- pnbd.mcmc.DrawParameters(cbs, chains = 4, mcmc = 50000, burnin = 5000)
  
  # compute median across draws for 4 parameters
  pnbd.cohort.draws <- pnbd.draws$level_2
  (params.pnbd.hb.median <- apply(as.matrix(pnbd.cohort.draws), 2, median))
  
  if(graph) {
    if(!dir.exists(log_path)) {
      dir.create(log_path, FALSE, TRUE)
    }
    pdf(paste(log_path, "/pnbdHB_freq_in_cal_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  freq.pnbd.hb <- mcmc.PlotFrequencyInCalibration(draws = pnbd.draws, cal.cbs = cbs,
                                                  censor = censor)
  if(graph) {
    dev.off()
  }
  freq.pnbd.hb <- t(as.data.table(freq.pnbd.hb))
  chisq.stat(freq.pnbd.hb, "Pareto/NBD (HB)", censor, 4)
  
  pnbd.xstar.draws <- mcmc.DrawFutureTransactions(cal.cbs = cbs, draws = pnbd.draws)
  xstar_temp <- cbs[,.(cust, x.star)]
  xstar_temp$xstar.pnbd.hb <- apply(pnbd.xstar.draws, 2, mean)
  
  writeLines("Pareto/NBD (HB): MAE")
  mae.pnbd.hb <- mae(act = xstar_temp$x.star, est = xstar_temp$xstar.pnbd.hb)
  print(mae.pnbd.hb)
  print(summary(xstar_temp$x.star - xstar_temp$xstar.pnbd.hb))
  
  if(graph) {
    pdf(paste(log_path, "/pnbdHB_mcmc_trace_plot_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  # plot trace- and density-plots for heterogeneity parameters
  # op <- par(mfrow = c(2,4), mar= c(2.5,2.5,2.5,2.5))
  mcmc.diag.lv2(pnbd.draws$level_2)
  mcmc.diag.lv1(pnbd.draws$level_1)
  if(graph) {
    dev.off()
  }
  
  if(graph) {
    pdf(paste(log_path, "/pnbdHB_no_of_txn_tracking_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  nil.pnbd.hb <- mcmc.PlotTrackingInc(draws = pnbd.draws, T.cal = cbs$T.cal,
                                      T.tot = max(cbs$T.cal + cbs$T.star),
                                      actual.inc.tracking.data = elog2inc(as.data.frame(elog)))
  if(graph) {
    dev.off()
    sink()
  }
  if(params.save) {
    saveRDS(pnbd.draws, paste(log_path, "/pnbdHB.rds", sep = ""), compress = "xz")
  }
  return(pnbd.draws)
}

fit.bgnbd <- function(cbs, elog, censor = 10, graph = FALSE, params.save = FALSE, 
                      log_path = paste("Logs/", format(Sys.time(), "%Y%m%d"), sep = "")) {
  if(graph) {
    if(!dir.exists(log_path)) {
      dir.create(log_path, FALSE, TRUE)
    }
    sink(paste(log_path, "/bgnbd_train_", format(Sys.time(), "%Y%m%d_%H%M"), ".log", sep = ""))
  }
  # estimate BG/NBD parameters
  params.bgnbd <- bgnbd.EstimateParameters(cbs) # BG/NBD
  LL.bgnbd <- bgnbd.cbs.LL(params.bgnbd, as.data.frame(cbs))
  
  if(graph) {
    if(!dir.exists(log_path)) {
      dir.create(log_path, FALSE, TRUE)
    }
    pdf(paste(log_path, "/bgnbd_freq_in_cal_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  freq.bgnbd <- bgnbd.PlotFrequencyInCalibration(params = params.bgnbd, 
                                                 cal.cbs = cbs, censor = censor)
  if(graph) {
    dev.off()
  }
  freq.bgnbd <- t(as.data.table(freq.bgnbd))
  chisq.stat(freq.bgnbd, "BG/NBD", censor, length(params.bgnbd))
  
  xstar.bgnbd <- bgnbd.ConditionalExpectedTransactions(params = params.bgnbd,
                                                       T.star = cbs$T.star, x = cbs$x,
                                                       t.x = cbs$t.x, T.cal = cbs$T.cal)
  mae.bgnbd <- mae(act = cbs$x.star, est = xstar.bgnbd)
  writeLines("BG/NBD: MAE")
  print(mae.bgnbd)
  print(summary(cbs$x.star - xstar.bgnbd))
  
  if(graph) {
    pdf(paste(log_path, "/bgnbd_no_of_txn_tracking_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  nil.bgnbd <- bgnbd.PlotTrackingInc(params = params.bgnbd, T.cal = cbs$T.cal, 
                                     T.tot = max(cbs$T.cal + cbs$T.star),
                                     actual.inc.tracking.data = elog2inc(as.data.frame(elog)))
  if(graph) {
    dev.off()
    sink()
  }
  if(params.save) {
    saveRDS(params.bgnbd, paste(log_path, "/bgnbd.rds", sep = ""), compress = "xz")
  }
  return(params.bgnbd)
}

fit.mbgnbd <- function(cbs, elog, censor = 10, graph = FALSE, params.save = FALSE, 
                       log_path = paste("Logs/", format(Sys.time(), "%Y%m%d"), sep = "")) {
  if(graph) {
    if(!dir.exists(log_path)) {
      dir.create(log_path, FALSE, TRUE)
    }
    sink(paste(log_path, "/mbgnbd_train_", format(Sys.time(), "%Y%m%d_%H%M"), ".log", sep = ""))
  }
  # under the bgnbd model P[Customer Alive] = 1 for a customer with zero repeat purchases in time period (0,T]
  # since we will use the modifyed bgnbd to replace the bgnbd model
  # estimate MBG/NBD parameters
  params.mbgnbd <- mbgnbd.EstimateParameters(cbs) # BG/NBD
  LL.mbgnbd <- mbgcnbd.cbs.LL(params.mbgnbd, as.data.frame(cbs))
  
  if(graph) {
    if(!dir.exists(log_path)) {
      dir.create(log_path, FALSE, TRUE)
    }
    pdf(paste(log_path, "/mbgnbd_freq_in_cal_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  freq.mbgnbd <- mbgcnbd.PlotFrequencyInCalibration(params = params.mbgnbd, 
                                                    cal.cbs = cbs, censor = censor)
  if(graph) {
    dev.off()
  }
  freq.mbgnbd <- t(as.data.table(freq.mbgnbd))
  chisq.stat(freq.mbgnbd, "MBG/NBD", censor, length(params.mbgnbd))
  
  xstar.mbgnbd <- mbgcnbd.ConditionalExpectedTransactions(params = params.mbgnbd,
                                                          T.star = cbs$T.star, x = cbs$x,
                                                          t.x = cbs$t.x, T.cal = cbs$T.cal)
  mae.mbgnbd <- mae(act = cbs$x.star, est = xstar.mbgnbd)
  writeLines("MBG/NBD: MAE")
  print(mae.mbgnbd)
  print(summary(cbs$x.star - xstar.mbgnbd))
  
  if(graph) {
    pdf(paste(log_path, "/mbgnbd_no_of_txn_tracking_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  nil.mbgnbd <- mbgcnbd.PlotTrackingInc(params = params.mbgnbd, T.cal = cbs$T.cal, 
                                        T.tot = max(cbs$T.cal + cbs$T.star),
                                        actual.inc.tracking.data = elog2inc(as.data.frame(elog)))
  if(graph) {
    dev.off()
    sink()
  }
  if(params.save) {
    saveRDS(params.mbgnbd, paste(log_path, "/mbgnbd.rds", sep = ""), compress = "xz")
  }
  return(params.mbgnbd)
}

fit.pggg <- function(cbs, elog, censor = 10, graph = FALSE, params.save = FALSE, 
                     log_path = paste("Logs/", format(Sys.time(), "%Y%m%d"), sep = "")) {
  if(graph) {
    if(!dir.exists(log_path)) {
      dir.create(log_path, FALSE, TRUE)
    }
    sink(paste(log_path, "/pggg_train_", format(Sys.time(), "%Y%m%d_%H%M"), ".log", sep = ""))
  }
  # generate parameters draws under pareto/GGG
  pggg.draws <- pggg.mcmc.DrawParameters(cbs, chains = 4, mcmc = 50000, burnin = 5000)
  pggg.cohort.draws <- pggg.draws$level_2
  (params.pggg.median <- apply(as.matrix(pggg.cohort.draws), 2, median))
  
  if(graph) {
    if(!dir.exists(log_path)) {
      dir.create(log_path, FALSE, TRUE)
    }
    pdf(paste(log_path, "/pggg_freq_in_cal_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  freq.pggg <- mcmc.PlotFrequencyInCalibration(draws = pggg.draws, cal.cbs = cbs, censor = censor)
  if(graph) {
    dev.off()
  }
  
  freq.pggg <- t(as.data.table(freq.pggg))
  chisq.stat(freq.pggg, "PGGG", censor, 4)
  
  pggg.xstar.draws <- mcmc.DrawFutureTransactions(cal.cbs = cbs, draws = pggg.draws)  
  xstar_temp <- cbs[,.(cust, x.star)]
  xstar_temp$xstar.pggg <- apply(pggg.xstar.draws, 2, mean)
  mae.pggg <- mae(act = xstar_temp$x.star,est = xstar_temp$xstar.pggg)
  writeLines("PGGG: MAE")
  print(mae.pggg)
  print(summary(xstar_temp$x.star - xstar_temp$xstar.pggg))
  
  # plot trace- and density-plots for heterogeneity parameters
  if(graph) {
    pdf(paste(log_path, "/pggg_mcmc_trace_plot_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  # op <- par(mfrow = c(2,4),mar= c(2.5,2.5,2.5,2.5))
  mcmc.diag.lv2(pggg.draws$level_2)
  mcmc.diag.lv1(pggg.draws$level_1)
  if(graph) {
    dev.off()
  }
  
  if(graph) {
    pdf(paste(log_path, "/pggg_no_of_txn_tracking_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = ""))
  }
  nil.pggg <- mcmc.PlotTrackingInc(draws = pggg.draws, T.cal = cbs$T.cal,
                                   T.tot = max(cbs$T.cal + cbs$T.star),
                                   actual.inc.tracking.data = elog2inc(as.data.frame(elog)))
  if(graph) {
    dev.off()
    sink()
  }
  if(params.save) {
    saveRDS(pggg.draws, paste(log_path, "/pggg.rds", sep = ""), compress = "xz")
  }
  return(pggg.draws)
}

mcmc.diag.lv1 <- function(lv1.draws, ncust_sel = 100){
  cust_sel <- sample(length(lv1.draws), ncust_sel, replace = FALSE)
  lapply(cust_sel, function(cust_idx) {
    subtitle <- paste("Customer ID:", names(lv1.draws)[cust_idx])
    draws_sel <- lv1.draws[[cust_idx]]
    coda::traceplot(draws_sel, sub=subtitle)
    coda::densplot(draws_sel, sub=subtitle)
    print(coda::acfplot(draws_sel, sub=subtitle))
    # coda::geweke.plot(coda::as.mcmc(draws_sel), sub=subtitle)
  })
}

mcmc.diag.lv2 <- function(lv2.draws){
  coda::traceplot(lv2.draws)
  coda::densplot(lv2.draws)
  print(coda::acfplot(lv2.draws))
  # coda::geweke.plot(coda::as.mcmc(lv2.draws))
}

elog2cumWithDate <- function (elog, by = 7, first = FALSE) 
{
  t0 <- N <- cust <- NULL
  stopifnot("cust" %in% names(elog))
  stopifnot(is.logical(first) & length(first) == 1)
  is.dt <- is.data.table(elog)
  if (!is.dt) {
    elog <- as.data.table(elog)
  }
  else {
    elog <- copy(elog)
  }
  if (!"t" %in% names(elog)) {
    stopifnot("date" %in% names(elog))
    cohort_start <- min(as.numeric(elog$date))
    elog[, `:=`(t, as.numeric(date) - cohort_start)]
  }
  elog_temp <- copy(elog)
  date_mapping <- data.table(t = 0:ceiling(max(elog$t)), date=seq(min(elog$date),max(elog$date),by=1))
  elog <- unique(elog[, list(cust, t)])
  elog[, `:=`(t0, min(t)), by = "cust"]
  grid <- data.table(t = 0:ceiling(max(elog$t)))
  grid <- merge(grid, elog[first | t > t0, .N, keyby = list(t = ceiling(t))], 
                all.x = TRUE, by = "t")
  grid <- grid[is.na(N), `:=`(N, 0L)]
  grid <- grid[, cum := cumsum(N)]
  idx_selected <- seq(by, nrow(grid), by = by)
  cum <- grid[idx_selected]
  cum <- merge(cum, date_mapping[idx_selected], by = "t")
  return(cum[,.(date, cum)])
}

elog2incWithDate <- function (elog, by = 7, first = FALSE) 
{
  cum <- elog2cumWithDate(elog = elog, by = by, first = first)
  return(data.table(date = cum$date[-1], cum=diff(cum$cum)))
}

getPredictionPeriod <- function(units, nperiod) {
  if (grepl("week", units)) {
    delta.t <- 1
    days <- 7
  } else if (grepl("month", units)) {
    delta.t <- 30 / 7
    days <- 30
  } else if (grepl("year", units)) {
    delta.t <- 365/7
    days <- 365
  }
  if (is.null(nperiod)) {
    nperiod <- 52
  }
  previous.time <- seq(from = 0, by = delta.t, length.out = nperiod)
  last.time <- seq(from = delta.t, by = delta.t, length.out = nperiod)
  return(list(period = days, begin = previous.time, end = last.time))
}

futureTxnLevel.threshold <- function(futureTxn, thres = 1e-4) {
  futureTxn[futureTxn < thres]$futureTxn <- 0
  return(futureTxn)
}

futureTxnLevel <- function(cbs, params, method = c("pnbd", "bgnbd", "mbgnbd", "pnbd.hb", "pggg"), 
                           nperiod = 13, byCustomer = FALSE) {
  method <- match.arg(method)
  if (tolower(method) %in% c("pnbd", "bgnbd", "mbgnbd")) {
    futureTxn <- cbs[, .(cust, x, t.x, T.cal)]
    futureTxn <- futureTxn[, .(futureTxn = futureTxnLevel.mle(params, x, t.x,
                                                              T.cal, nperiod, method)), by = .(cust)]
  } else {
    cust_sel <- names(draws$level_1) # Select customers that have draws
    futureTxn <- apply(mcmc.DrawFutureTransactions(cal.cbs = cbs, draws = params, T.star = nperiod), 
                       2, mean)
    futureTxn <- data.table(cust = cust_sel, futureTxn = futureTxn)
  }
  if (!byCustomer) {
    return(round(sum(futureTxnLevel.threshold(futureTxn)$futureTxn)))
  } else {
    return(futureTxnLevel.threshold(futureTxn))
  }
}

futureTxnLevel.mle <- function(params, x, t.x, T.cal, T.star, 
                               method = c("pnbd", "bgnbd", "mbgnbd")) {
  method <- match.arg(method)
  return(switch(method, # TODO: modify the code to avoid overflow when the x is large, say > 100.
                pnbd = pnbd.ConditionalExpectedTransactions(params, T.star, x, t.x, T.cal),
                bgnbd = bgnbd.ConditionalExpectedTransactions(params, T.star, x, t.x, T.cal),
                mbgnbd = mbgcnbd.ConditionalExpectedTransactions(params, T.star, x, t.x, T.cal)))
}

futureTxnLevel.mcmc <- function(cal.cbs, draws, T.star, method = c("pnbd.hb","pggg")) {
  if (!tolower(method) %in% c("pnbd.hb","pggg")){
    stop("method is not valid")
  }
  # Calculate the number of cores
  no_cores <- min(max(1, detectCores() - 1), length(T.star))
  # Initiate cluster
  cl <- makeCluster(no_cores)
  futureTxnDraws <- parLapply(cl, T.star, function(x, cal.cbs, draws) {
    set.seed(7)
    return(apply(BTYDplus::mcmc.DrawFutureTransactions(cal.cbs = cal.cbs, draws = draws, T.star = x), 2, mean))
  }, cal.cbs = cal.cbs, draws = draws)
  stopCluster(cl)
  return(do.call(rbind, futureTxnDraws))
}

plotTrackingInc <- function (params, cbs, elog, customerSegments = NULL, 
                             method = c("pnbd", "bgnbd", "mbgnbd", "pnbd.nb", "pggg"),
                             unit = "weeks", lab = "Date", ylab = "Transactions", 
                             xticklab = NULL, title = "Tracking Weekly Transactions",
                             npredict = NULL) 
{
  expectedCumulativeTransactions <- switch(method, 
                                           pnbd = pnbd.ExpectedCumulativeTransactions,
                                           bgnbd = bgnbd.ExpectedCumulativeTransactions,
                                           mbgnbd = mbgcnbd.ExpectedCumulativeTransactions,
                                           pnbd.nb = mcmc.ExpectedCumulativeTransactions,
                                           pggg = mcmc.ExpectedCumulativeTransactions)
  plotting <- function(df, segment = NULL, size = 12) {
    g <- ggplot(df, aes(x=Date, y=value, col=variable, linetype=variable)) + geom_line(size=1)
    if (is.null(npredict)) {
      g <- g + geom_vline(xintercept = as.numeric(min(elog$date) + weeks(max(cbs$T.cal))), linetype="dotted", size=1)
    }
    if (!is.null(segment)) {
      g <- g + labs(title=title, y=ylab, subtitle=segment)   # title and caption
    } else {
      g <- g + labs(title=title, y=ylab)   # title and caption
    }
    g <- g + scale_x_date(date_breaks = "1 month") + theme_bw() + 
      theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = size),  # rotate x axis text
            axis.text.y = element_text(size = size),
            axis.title.x = element_text(size=size), axis.title.y = element_text(size=size),
            panel.grid.minor = element_blank(), panel.grid = element_blank(),  # turn off minor grid
            legend.title = element_blank(), legend.text = element_text(size = size),
            legend.key.size = unit(1, "cm"), 
            plot.title = element_text(size = size), plot.subtitle = element_text(size = size))
    return(g)
  }
  
  if (!is.null(customerSegments)) {
    custSegments <- unique(customerSegments$Segment)
    gList <- lapply(custSegments, function(segment) {
      selected_customers <- customerSegments[Segment == segment, CustomerID]
      cbs_selected <- cbs[cust %in% selected_customers]
      T.cal <- cbs_selected$T.cal
      T.tot <- max(cbs_selected$T.cal + cbs_selected$T.star)
      
      actual <- elog2inc(as.data.frame(elog[cust %in% selected_customers]))
      expected <- dc.CumulativeToIncremental(expectedCumulativeTransactions(
        params, T.cal, T.tot, length(actual)))
      
      dates <- seq(min(elog$date), max(elog$date), unit)[-1]
      df <- data.table(Date = dates, variable = as.factor("Actual"), value = actual)
      df <- rbind(df, data.table(Date = dates, variable = as.factor("Expected"), value = expected))
      g <- plotting(df, segment)
      return(g)
    })
    return(gList)
  } else {
    T.cal <- cbs$T.cal
    actual <- elog2incWithDate(as.data.frame(elog[cust %in% cbs$cust]))
    if (!is.null(npredict)) {
      predictionPeriod <- getPredictionPeriod(unit, npredict)
      predictionPeriod <- sort(unique(c(predictionPeriod$begin,predictionPeriod$end)))
      no_cores <- min(max(1, detectCores() - 1), length(predictionPeriod)) # Calculate the number of cores
      cl <- makeCluster(no_cores) # Initiate cluster
      clusterEvalQ(cl, {
        library(data.table)
        library(BTYD)
        library(BTYDplus)
        source("../LTV/BTYDmodels.R")
      })
      # clusterExport(cl, list("futureTxnLevel"))
      clusterExport(cl, list("cbs", "params", "method"), envir=environment())
      delta.txn <- parSapply(cl, predictionPeriod, function(nperiod) {
        output <- futureTxnLevel(cbs, params, method, nperiod, TRUE)
        return(output$futureTxn)
      })
      stopCluster(cl)
      delta.txn <- t(diff(t(delta.txn)))
      expected <- c(rep(NA, nrow(actual)), colSums(delta.txn))
      predictionDates <- seq(max(actual$date), length.out = npredict + 1, by = unit)[-1]
      actual <- rbind(actual, data.table(date = predictionDates, cum = NA))
      legendLabel <- "Prediction"
    } else {
      T.tot <- max(cbs$T.cal + cbs$T.star)
      expected <- dc.CumulativeToIncremental(expectedCumulativeTransactions(
        params, T.cal, T.tot, nrow(actual)))
      legendLabel <- "Expected"
    }
    df <- cbind(actual,expected)
    setnames(df, c("date", "cum", "expected"), c("Date", "Actual", legendLabel))
    df$Actual <- as.numeric(df$Actual)
    df <- data.table::melt(df, "Date")
    df <- na.omit(df)
    g <- plotting(df)
    return(g)
  }
}

findPAlive <- function(cbs, params, method = c("pnbd", "bgnbd", "mbgnbd", "pnbd.nb", "pggg"), thres = 1e-4) {
  method <- match.arg(method)
  output <- cbs[, .(pAlive = palive(params, x, t.x, T.cal, method)), by = .(cust)]
  output[pAlive < thres]$pAlive <- 0
  return(output)
}

palive <- function(params, x, t.x, T.cal, method = c("pnbd", "bgnbd", "mbgnbd", "pnbd.nb", "pggg")) {
  method <- match.arg(method)
  return(switch(method, # TODO: modify the code to avoid overflow when the x is large, say > 100.
                pnbd = pnbd.PAlive(params, x, t.x, T.cal),
                bgnbd = bgnbd.PAlive(params, x, t.x, T.cal),
                mbgnbd = mbgcnbd.PAlive(params, x, t.x, T.cal),
                pnbd.nb = mcmc.PAlive(params),
                pggg = mcmc.PAlive(params)))
}

nActiveCust <- function(pAlive) {
  return(round(sum(pAlive$pAlive, na.rm = TRUE)))
}

rocActiveCust <- function(elog, params, method = c("pnbd", "bgnbd", "mbgnbd", "pnbd.nb", "pggg"),
                          period = 13, units = "weeks") {
  method <- match.arg(method)
  T.cal.old <- max(elog$date) - as.difftime(period, units = units)
  nActiveCust.old <- nActiveCust(findPAlive(getCBS(elog, T.cal=T.cal.old, units=units), 
                                            params, method))
  nActiveCust.new <- nActiveCust(findPAlive(getCBS(elog, 0, units=units), 
                                            params, method))
  roc <- (nActiveCust.new - nActiveCust.old)/nActiveCust.old
  return(roc)
}

nRegCust <- function(cbs, T.end) {
  return(length(unique(cbs[first <= T.end]$cust)))
}

rocRegCust <- function(cbs, T.tot, period = 13, units = "weeks") {
  T.old <- as.Date(T.tot) - as.difftime(period, units = units)
  nRegCust.old <- nRegCust(cbs, T.old)
  nRegCust.new <- nRegCust(cbs, T.tot)
  roc <- nRegCust.old
  roc <- (nRegCust.new - roc)/roc
  return(roc)
}

rocActivePerRegCust <- function(elog, params, method = c("pnbd", "bgnbd", "mbgnbd", "pnbd.nb", "pggg"),
                                period = 13, units = "weeks") {
  method <- match.arg(method)
  T.old <- max(elog$date) - as.difftime(period, units = units)
  cbs.old <- getCBS(elog, T.cal=T.old, units=units)
  nActiveCust.old <- nActiveCust(findPAlive(cbs.old, params, method))
  cbs.new <- getCBS(elog, 0, units=units)
  nActiveCust.new <- nActiveCust(findPAlive(cbs.new, params, method))
  nRegCust.old <- nRegCust(cbs.old, T.old)
  nRegCust.new <- nRegCust(cbs.new, max(elog$date))
  roc <- nActiveCust.old/nRegCust.old
  roc <- ((nActiveCust.new/nRegCust.new) - roc)/roc
  return(roc)
}