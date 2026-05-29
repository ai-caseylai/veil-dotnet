library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(purrr)
library(plotly)

elog2iet <- function(elog, units = "week") {
  # Adopted from BTYDplus::elog2cbs
  cust <- first <- itt <- sales  <- NULL
  stopifnot(inherits(elog, "data.frame"))
  if (!all(c("cust", "date") %in% names(elog))) 
    stop("`elog` must have fields `cust` and `date`")
  if (!any(c("Date", "POSIXt") %in% class(elog$date))) 
    stop("`date` field must be of class `Date` or `POSIXt`")
  if ("sales" %in% names(elog) & !is.numeric(elog$sales)) 
    stop("`sales` field must be numeric")
  is.dt <- is.data.table(elog)
  has.sales <- "sales" %in% names(elog)
  elog_dt <- data.table(elog)
  setkey(elog_dt, cust, date)
  if (!has.sales) {
    elog_dt[, `:=`(sales, 1)]
  }
  else {
    stopifnot(is.numeric(elog_dt$sales))
  }
  global_first <- min(elog_dt$date)
  global_end <- max(elog_dt$date)
  periods_total <- as.numeric(difftime(global_end, global_first, units = units))
  # Remove transaction at the begining of the period
  elog_dt <- elog_dt[date != global_first]
  elog_dt <- elog_dt[, first := min(date), by = .(cust)]
  # The period start at 1 rather than 0
  elog_dt <- elog_dt[, t := as.numeric(difftime(date, global_first, units = units)), by = .(cust)]
  return(list(N = periods_total, dt = computeIET(elog_dt, periods_total)))
}

computeIET <- function(dt, N) {
  # dt contains attributes: cust, t
  dt_copy <- data.table(dt)
  if(any(is.integer(dt_copy$t))) {
    dt_copy <- dt_copy[, t := as.numeric(t)]
  }
  setorderv(dt_copy, c("cust", "t"))
  dt_copy <- dt_copy[, itt := c(0, diff(t)), by = .(cust)]
  dt_copy <- dt_copy[, .(cust, t, itt)]
  setorderv(dt_copy, c("cust", "t"))
  dt_copy <- dt_copy[itt != 0, x := itt]
  dt_copy <- dt_copy[itt == 0, x := t]
  dt_copy <- dt_copy[, i := rowid(cust)]
  
  last_x_dt <- dt_copy[, .(i = max(i)+1, x = N+1-max(t)), by = .(cust)]
  itt_dt <- dt_copy[, .(cust, i, x)]
  itt_dt <- rbind(itt_dt, last_x_dt)
  setorderv(itt_dt, c("cust", "i"))
  itt_dt <- itt_dt[, n.x := x/(N+1)]
  return(itt_dt)
}

clumpiness <- function(x) {
  return(1+sum(log(x) * x)/(log(length(x))))
}

clumpinessSampling <- function(n, N, M = 10000) {
  stopifnot(N >= n)
  set.seed(7)
  samples <- data.table(cust = (1:M))
  samples <- samples[, list(t = sample(x = (1:N), size = n, replace = FALSE)), by = .(cust)]
  setorder(samples, cust, t)
  iet_dt <- computeIET(samples, N)
  clumpiness_dt <- iet_dt[, .(clumpiness = clumpiness(n.x)), by = .(cust)]
  return(clumpiness_dt$clumpiness)
}

clumpinessCritiVal <- function(n, N, alpha = 0.05, M = 10000, rds.path = "./rds/clumpinessSamples.rds") {
  clumpiness_sample <- NULL
  if (file.exists(rds.path)) {
    findSample <- function(sampleList, n, N) {
      idx <- which(sapply(sampleList, function(x) (x$N==N && x$n == n)))
      if (length(idx) == 0) {
        output <- clumpinessSampling(n, N, M)
        sampleList[[length(sampleList)+1]] <- list(N = N, n = n, samples = output)
        saveRDS(sampleList, rds.path)
        return(output)
      } else {
        return(sampleList[[idx]]$samples)
      }
    }
    samplesList <- readRDS(rds.path)
    clumpiness_sample <- findSample(samplesList, n, N)
  } else {
    clumpiness_sample <- clumpinessSampling(n, N, M)
    sampleList <- NULL
    sampleList[[1]] <- list(N = N, n = n, samples = clumpiness_sample)
    if(!dir.exists(rds.path)) {
      rds_dir <- unlist(strsplit(file.path(rds.path), "/"))
      rds_dir <- rds_dir[-length(rds_dir)]
      rds_dir <- paste0(rds_dir, collapse = "/")
      dir.create(rds_dir, showWarnings = FALSE, recursive = TRUE)
    }
    saveRDS(sampleList, rds.path, compress = "xz")
  }
  return(quantile(clumpiness_sample, probs = 1-alpha, names = FALSE))
}

plotEvent <- function(elog_selected, begin, end, units = "days") {
  if(!(is.Date(begin) && is.Date(end))) {
    begin <- as.Date(begin)
    end <- as.Date(end)
  }
  g <- ggplot(elog_selected, aes(x = cust, y = date))
  g <- g + geom_point(col="tomato2", size=2)   # Draw points
  g <- g + geom_segment(aes(x=cust, xend=cust, 
                            y=begin, yend=max(end, date)), 
                        linetype="dashed", 
                        lineend = "butt",
                        size=0.1)   # Draw dashed lines
  g <- g + labs(title="Event Plot") +  ylab("Date") + xlab("Customer")
  if (units == "weeks") {
    g <- g + scale_y_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(1e-3,1e-3))
  } else {
    g <- g + scale_y_date(date_labels = "%Y-%m", date_breaks = "1 month", expand = c(1e-3,1e-3))
  }
  g <- g + coord_flip()
  g <- g + theme(axis.text.x=element_text(angle=45, hjust=1),
                 axis.text.y = element_text(angle=0, hjust=1))
  return(g)
}

plotEventDetail <- function(txnData, selectedCustomer = NULL) {
  if (is.null(selectedCustomer)) {
    selectedCustomer <- sample(unique(presentRv$txnData$MemberID), 50)
  }
  mytxn <- txnData[MemberID %in% selectedCustomer]
	mytxn <- mytxn %>% mutate(
		pdt = paste0("Product:(",ProductID,",",ProductName,") Qty:",Quantity," UnitPx:",UnitPrice)
	) %>% group_by(OrderID) %>% summarise(
	  Date = as.POSIXct(first(Timestamp)),
	  Member = as.character(first(MemberID)),
	  Details = paste0(pdt,collapse="\n"),
	  Amount = max(TxnTotalAmt),
	  Store = as.factor(first(StoreID))
	) %>% mutate(
	  Details = paste(paste0("Amt: ",Amount),Details,sep="\n")
	)
	return(plot_ly(mytxn) %>% add_markers(x=~Date,y=~Member,size=~Amount,hovertext=~Details))
}

plotEventDetailByStore <- function(txnData) {
  mytxn <- txnData %>% group_by(StoreID, OrderID) %>% summarise(
    Date = as.Date(first(Timestamp)),
    Amount = sum(TxnTotalAmt)
  ) %>% mutate(
    Details = paste(paste0("Amt: ",Amount),sep="\n")
  )
  plot_ly(mytxn) %>% add_markers(x=~Date,y=~StoreID,hovertext=~Details)
}