library(data.table)
library(dplyr) # version >= 0.7.1
source("../Utils/master.R")
##########################################################################################
# This part is for data pre-processing
##########################################################################################

sevenFans.cleandata <- function(data) {
  # Drop the 7-11 staff accounts (CardNumber begin with 9).
  if ("CarNumber" %in% colnames(data)) {
    data <- data[substr(CardNumber,1,1) != "9" & CardNumber != ""]
  }
  if ("Category" %in% colnames(data)) {
    data <- data[Category != "SER9901"]
  }
  if ("NetPrice" %in% colnames(data)) {
    data <- data[NetPrice > 0]
  }
  # Remove the items contain keywords: $ SAVE, SAVE $, CPN,COUPON, BOTTLE RETURN, 
  #										bottle refund 7, DIS CARD 8, NEWSPAPER OFF 9, #TAKE-OUT
  if ("ProductName" %in% colnames(data)) {
    productNameRegExp <- "(\\$).*(SAVE)|(SAVE).*(\\$)|(CPN)|(COUPON)|(BOTTLE).*(RETURN)|
  (bottle).*(refund)|(DIS CARD)|(NEWSPAPER).*(OFF)|(#TAKE-OUT)"
    data <- data[!grepl(productNameRegExp, ProductName)]
  }
  return (data)
}

sevenFans.readtxn <- function(file, datetime=FALSE, select = NULL, begin = NULL, end = NULL) {
  colType <- c(TxnNo = "character", TxnDate = "character", MemberID = "character",
               CardNumber = "character", TenderCode = "character", TotalAmount = "numeric",
               SeqNo = "integer", RetailPrice = "numeric", NetPrice = "numeric", Qty ="integer",
               ProdCode = "character", ProdName1 = "character", DepartCode = "character",
               DepartName1 = "character")
  if (!grepl("http://", file)) {
    colnames_old <- c("TxnNo", "TxnDate", "Qty", "ProdCode", "DepartName1", "ProdName1")
    colnames_new <- c("OrderID", "Timestamp", "Quantity", "ProductID", "Category", "ProductName")
    if (!is.null(select)) {
      select_old <- select
      select_old[which(select %in% colnames_new)] <- colnames_old[which(colnames_new %in% select)]
      colType <- colType[names(colType) %in% select_old]
      txn <- fread(file, colClasses = colType, select = select_old, stringsAsFactors = FALSE, showProgress = FALSE)
      idx_selected <- which(colnames_old %in% select_old)
      colnames_old <- colnames_old[idx_selected]
      colnames_new <- colnames_new[idx_selected]
    } else {
      txn <- fread(file, colClasses = colType, stringsAsFactors = FALSE, showProgress = FALSE)
    }
    # Change the column names to reuse other functions
    setnames(txn, colnames_old, colnames_new)
  } else {
    # Change the column names to reuse other functions
    names(colType)[1] <- "OrderID"
    names(colType)[2] <- "Timestamp"
    names(colType)[10] <- "Quantity"
    names(colType)[11] <- "ProductID"
    names(colType)[12] <- "ProductName"
    names(colType)[14] <- "Category"
    if (!is.null(select)) {
      colType <- colType[names(colType) %in% select]
    } else {
      select <- names(colType)
    }
    address <- getUrl(address = file, type = "attributes", bu = 107, attributes = select, begin = begin, end = end)
    txn <- read.url(address, colClasses = colType, stringsAsFactors = FALSE, showProgress = FALSE)
  }
  txn <- sevenFans.cleandata(txn)
  if(datetime) {
    txn$Timestamp <- as.POSIXct(txn$Timestamp)
  }
  return(txn)
}

##########################################################################################
# This part is for Customer Lifetime Value
##########################################################################################

sevenFans.txn2elog <- function(txnData, units = "days"){
  return(txn2elog(txnData, units))
}

sevenFans.getElog <- function(txn, units = "days") {
  
  # aggregated the total amount for each transaction 
  txnTreated <- txn[,.(TxnAmt = sum(Quantity * NetPrice)), by = .(OrderID,MemberID,Timestamp)]
  return(getElog(txnTreated, units))
}

sevenFans.getHoldoutDate <- function(txnData, p = 0.2) {
  return(getHoldoutDate(txnData, p))
}

sevenFans.getAcquiredDate <- function(txnData, p = 0.2){
  return(getAcquiredDate(txnData, p))
}