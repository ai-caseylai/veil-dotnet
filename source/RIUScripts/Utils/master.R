library(data.table)
library(lubridate)
library(curl)

getUrl <- function(address, type, bu, attributes = NULL, output = "csv", begin = NULL, end = NULL, keys=NULL) {
  if (!grepl("http://", address)) {
    address <- paste("http://", address, sep = "")
  }
  if (substring(address, nchar(address)) != "/") {
    address <- paste(address, "/", sep = "")
  }
  address <- paste(address, "api?bu=", as.character(bu), sep = "")
  if (missing(type)) {
    if (bu != "holiday") {
      stop("type is not valid")
    }
  } else{
    if (type != "attributes") {
      address <- paste(address, "&type=", type, sep = "")
      if (!is.null(keys)) {
        keys <- paste(keys, collapse = ",")
        address <- paste(address, "&keys=", keys, sep = "")
      }
    } else {
      if (!is.null(attributes)) {
        attributes <- paste(attributes, collapse = ",")
        address <- paste(address, "&attributes=", attributes, sep = "")
      } else {
        stop("attributes is not valid")
      }
    }
  }
  if (all(!is.null(c(begin, end)))) {
    if (!is.null(begin)) {
      address <- paste(address, "&begin=", begin, sep = "")
    }
    if (!is.null(end)) {
      address <- paste(address, "&end=", end, sep = "")
    }
  }
  address <- paste(address, "&out=", output, sep = "")
  return(address)
}

read.url <- function(url, timeout = 3000, ...){
  if (!grepl("http://", url)) {
    tmpFile <- url
  } else {
    options(timeout = timeout)
    tmpFile <- tempfile()
    if (Sys.which("curl") != "") {
      download.file(url, destfile = tmpFile, method = "curl")
    } else {
      curl_download(url, destfile = tmpFile)
    }
  }
  url.data <- fread(tmpFile, ...)
  return(url.data)
}

extractTxnByTime <- function(txnData, period = "1 year"){
  # Transfrom the txn Data within a certain time period
  # Requirements: 'data.table', 'lubridate'
  #
  # Args:
  #   txnData: A data.table object
  #            (required column: 'Timestamp', transaction datetime in character).
  #   period: A character of either "1 day" "2 month" or "1 year"
  # Output:
  #   A data.table object
  
  if (class(txnData$Timestamp) != "Date") {
    txnData$Timestamp <- as.Date(txnData$Timestamp)
  }
  number <- as.integer(unlist(strsplit(period," "))[1])
  period <- unlist(strsplit(period," "))[2]
  end_Date <- max(txnData$Timestamp)
  if(!is.null(period) ){
    start_Date <- end_Date
    if(grepl("year", period)){
      year(start_Date) <- year(start_Date) - number
    } else if(grepl("month", period)){
      month(start_Date) <- month(start_Date) - number
    } else if(grepl("day", period)){
      day(start_Date) <- day(start_Date) - number
    }
  }
  txnData <- txnData[Timestamp > start_Date,]
  return(txnData)
}

##########################################################################################
# This part is for recommendation system
##########################################################################################

rankingFunc <- function(x, minCumQty, ranking.max) {
  quantiles <- seq(0, 1, length.out = ranking.max+1)[-1] # Omit the first term, 0
  # The following is the lowest class when the CumQty <= minCumQty.
  cutpoints <- minCumQty
  # The following checks if all values belongs to the first class
  if (any(x > minCumQty)) {
    # The following defines the class interval (left open, right closed) for each remaing quantiles.
    cutpoints <- c(cutpoints, unname(quantile(x[x > minCumQty], quantiles, na.rm = TRUE)))
  }
  # The following gets the corresponding interval for each input value x.
  # Since findInterval begins with 0, 1 is added.
  rankings <- findInterval(x, cutpoints, left.open = TRUE) + 1
  return(rankings)
}

##########################################################################################
# This part is for Customer Lifetime Value
##########################################################################################

getElog <- function(txnTreated, units = "days") {
  # txnTreated contains attributes: OrderID,MemberID,Timestamp, TxnAmt
  
  # remove the duplicated OrderID in the txnTreated
  duplicatedOrderID <- txnTreated[duplicated(txnTreated$OrderID),]$OrderID
  txnTreated <- txnTreated[!OrderID %in% duplicatedOrderID,]
  
  # aggregate the transactions within one day (default)
  elog <- txn2elog(txnTreated, units)
  return(elog)
}

aggregateTxn <- function(txnData, units = c("minutes", "hours",
                                            "days", "weeks")) {
  units <- match.arg(units)
  if (units != "days") {
    txnData <- txnData[, Timestamp := ceiling_date(Timestamp, units)]
  } else {
    txnData$Timestamp <- as.Date(txnData$Timestamp)
  }
  txnData <- txnData[,.(sales = sum(TxnAmt)), by = .(MemberID,Timestamp)]
  return(txnData)
}

txn2elog <- function(txnData, units = "days"){
  # some customers' intertransaction time are just 10 min or 1 hour
  # maybe there two transations can be aggregate to be one transations
  # arguments: 
  #        txnData is data.table 
  #        units is the aggregate type:
  # for example:
  # units = "day", max.length = 1 then the transactions within one day 
  #         is one txn for each customer
  txnData <- aggregateTxn(txnData, units)
  names(txnData) <- c("cust","date","sales")
  return(txnData)
}

getHoldoutDate <- function(txnData, p = 0.2) {
  # to get the first day of the holdout period
  maxDate <- max(txnData$date)
  minDate <- min(txnData$date)
  holdoutDate <- maxDate - (maxDate - minDate) * p
  # holdout peried will be the holdoutDate to the maxDate of the transations
  return(holdoutDate)
}

getAcquiredDate <- function(txnData, p = 0.2){
  # get the last day of acquire period
  # the customers in the acquired period will be selected to train the model
  maxDate <- max(txnData$date)
  minDate <- min(txnData$date)
  acquiredDate <- minDate + (maxDate - minDate) * p
  # acquired period will be the minDate of the transations to the acquiredDate
  return(acquiredDate)
}

checkDate <- function(date) {
  if (!is.null(date)) {
    while (is.na(as.Date(date, format = "%Y%m%d"))) {
      date <- as.character(as.integer(date) - 1L)
      warning("Date out of range, changed to previous date.")
    }
    return(as.character(date, format = "%Y%m%d"))
  } else {
    return(date)
  }
}

getNumberOfTxn <- function(address, bu, units = c("date", "week", "month"),
                           begin = NULL, end = NULL, key = NULL) {
  begin <- checkDate(begin)
  end <- checkDate(end)
  units <- match.arg(units)
  url <- getUrl(address = address, type = "frequency", bu = bu, begin = begin,
                end = end, keys = c(key, units))
  out_df <- read.url(url)
  out_df$Date <- as.Date(out_df$Date)
  out_df <- setorderv(out_df, "Date")
  return(out_df)
}

getNumberOfTxnByCategory <- function(address, bu, units = c("date", "week", "month"),
                                     begin = NULL, end = NULL, 
                                     categoryLevel = c(1, 2, 3)) {
  units <- match.arg(units)
  begin <- checkDate(begin)
  end <- checkDate(end)
  if (missing(categoryLevel)) {
    categoryLevel <- 3
  }
  dt <- getNumberOfTxn(address, bu, units, begin, end, key = "Category")
  if (categoryLevel == 1) {
    dt$Category <- substring(dt$Category, 1, 3)
  } else if (categoryLevel == 2) {
    dt$Category <- substring(dt$Category, 1, 5)
  } else {
    return(dt)
  }
  if (units == "date") {
    return(dt[,.(value = sum(value)), .(Date, Category)])
  } else if (units == "week") {
    dt$Date <- as.Date(dt$Date)
    return(dt[,.(Date = max(Date), value = sum(value)), .(`Year-Week`, Category)])
  } else if (units == "month") {
    return(dt[,.(Date = max(Date), value = sum(value)), .(`Year-Month`, Category)])
  }
  
}

getWeatherData <- function(address, units = c("date", "week", "month", "normal"),
                           aws = NULL, begin = NULL, end = NULL) {
  units <- match.arg(units)
  begin <- checkDate(begin)
  end <- checkDate(end)
  type <- switch(units,
                 "date" = "d",
                 "week" = "w",
                 "month" = "m",
                 "normal" = "n")
  url <- getUrl(address = address, bu = "hko", type = type, begin = begin, 
                end = end)
  if (!is.null(aws)) {
    url <- paste(url, "&aws=", aws, sep = "")
  }
  out_df <- read.url(url)
  out_df$Date <- as.Date(out_df$Date)
  out_df <- setorderv(out_df, "Date")
  return(out_df)
}

getHolidays <- function(address, begin, end, units = c("date", "week", "month"),
                        type = c("all", "weekend", "holiday")) {
  units <- match.arg(units)
  begin <- checkDate(begin)
  end <- checkDate(end)
  type <- match.arg(type)
  url <- getUrl(address = address, bu = "holiday", begin = begin, end = end, 
                type = type)
  df_out <- read.url(url)
  df_out$Date <- as.Date(df_out$Date)
  df_out <- setorderv(df_out, "Date")
  if (units == "week") {
    df_out[, "Year-Week"] <- gsub("-0", "-", 
                                  format(df_out[, "Date"], format = "%Y-%V"))
  } else if (units == "month") {
    df_out[, "Year-Month"] <- gsub("-0", "-", 
                                   format(df_out[, "Date"], format = "%Y-%m"))
  }
  return(df_out)
}
