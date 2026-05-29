library("data.table")
library("dplyr") # version >= 0.7.1
source("../Utils/master.R")
##########################################################################################
# This part is for data pre-processing
##########################################################################################

Bauhaus.cleanData <- function(data) {
  data <- data[ProductID !='PSB001']
  data <- data[Quantity > 0]
  return(data)
}

Bauhaus.readtxn <- function(file, datetime = FALSE, select = NULL, begin = NULL, end = NULL) {
  colType <- c(BU = "character", Timestamp = "character", SalesType = "character", 
               OrderID = "character", ProductID = "character", ProductName = "character", 
               Quantity = "integer", UnitPrice = "numeric", TotalPrice = "numeric", 
               TxnTotalAmt = "numeric", StoreCode = "character", StoreStatus = "character",
               MemberID = "character", MemberStatus = "character", Gender = "character", 
               MemberEmail = "character")
  if (!grepl("http://", file)) {
    colnames_old <- c("TotalPrice", "StoreCode")
    colnames_new <- c("NetPrice", "StoreID")
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
      txn <- Bauhaus.cleanData(txn)
    }
    setnames(txn, colnames_old, colnames_new)
  } else {
    # Change the column names to reuse other functions
    names(colType)[9] <- "NetPrice"
    names(colType)[11] <- "StoreID"
    if (!is.null(select)) {
      colType <- colType[names(colType) %in% select]
    } else {
      select <- names(colType)
    }
    address <- getUrl(address = file, type = "attributes", bu = 106, attributes = select, begin = begin, end = end)
    txn <- read.url(address, colClasses = colType, stringsAsFactors = FALSE, showProgress = FALSE)
  }
  if(datetime) {
    txn$Timestamp <- as.POSIXct(txn$Timestamp)
  }
  return(txn)
}

Bauhaus.extractTxnByTime <- function(txnData, period = "1 year"){
  # transfrom the txn Data within a certain time period for Bauhaus data
  # Args:
  # txnData: A data.table 
  # period: "1 day" "2 month" or "1 year"
  # require 'lubridate' package
  return(extractTxnByTime(txnData, period))
}

##########################################################################################
# This part is for recommendation system
##########################################################################################

Bauhaus.splitCategory <- function(data){
  # split derivedCategory to single Category
  dataInCat <- data[, .(Category = unlist(strsplit(derivedCategory, '%:@'))),
                    by = c("OrderID","derivedCategory")]
  setkey(data, OrderID, derivedCategory)
  setkey(dataInCat, OrderID, derivedCategory)
  dataModified <- merge(data, dataInCat, all.y = TRUE)
  return(dataModified)
}

Bauhaus.filterCategory <- function(data, categoryThreshold = 200){
  # remove the category which quantity is less than 200
  Category <- data[, .(sumQty = sum(Quantity)), by = "Category"]
  Category <- unique(Category[, .(Category, sumQty)])
  filteredCategory <- Category[sumQty >= categoryThreshold]$Category
  data <- data[Category %in% filteredCategory]
  return(data)
}

Bauhaus.aggregate <- function(mydata,key=names(mydata)[1:2]) {
  return(mydata[,.(CumQty=sum(Quantity)),by=key])
}

Bauhaus.rankingFunc <- function(x, minCumQty, ranking.max) {
  return(rankingFunc(x, minCumQty, ranking.max))
  # quantiles <- seq(0, 1, length.out = ranking.max+1)[-1] # Omit the first term, 0
  # # The following is the lowest class when the CumQty <= minCumQty.
  # cutpoints <- minCumQty
  # # The following checks if all values belongs to the first class
  # if (any(x > minCumQty)) {
  #   # The following defines the class interval (left open, right closed) for each remaing quantiles.
  #   cutpoints <- c(cutpoints, unname(quantile(x[x > minCumQty], quantiles, na.rm = TRUE)))
  # }
  # # The following gets the corresponding interval for each input value x.
  # # Since findInterval begins with 0, 1 is added.
  # rankings <- findInterval(x, cutpoints, left.open = TRUE) + 1
  # return(rankings)
}

Bauhaus.transform <- function(dataCumQty, rankingMax = 3, 
                             userSumThreshold = 20, minCumQty = 2) {
  # Data transformation
  # Transform the data from transaction record to cumulative quantity to ranking
  #
  # Args:
  #   dataCumQty:       Data (base::matrix, base::data.frame, data.table::data.table)
  #   rankingMax:       The maximum ranking - 1 for the data transformation (numeric)
  #   userSumThreshold: The user who bought items in quantity less than this value 
  #                     will be dropped (numeric)
  #   minCumQty:        Cumulative quantity less than this value is assigned 
  #                     to have ranking of 1
  # Returns:
  #   Transformed data (base::matrix, base::data.frame, data.table::data.table)
  
  # The threshold changed from cumulative qunatities to sum of distinct items
  distinctItemSum <- function(x){
    return(length(x))
  }
  userTable <- dataCumQty[, .(userSum=distinctItemSum(CumQty)), by = MemberID]
  selectedMembers <- userTable[userSum >= userSumThreshold,MemberID]
  dataSelected <- dataCumQty[MemberID %in% selectedMembers]
  dataSelected[, ranking:=Bauhaus.rankingFunc(CumQty,minCumQty,rankingMax),by=MemberID]
  return(dataSelected[,CumQty:=NULL])
}

Bauhaus.getIndexTable <- function(vals) {
  # analysis requires matrix where indices are integers. So this function return a data.table to
  # help getting the index for each value in a given column
  # be a data.table
  items <- sort(unique(as.data.frame(vals)[,1]))
  dt<-data.table(item=items,idx=1:length(items))
  return(data.table::setkey(dt,item))
}

# addIndices as well as getIndexTable can be reused with any data.table
Bauhaus.addIndices <- function(data,rowID=colnames(data)[1],colID=colnames(data)[2]) {
  # add columns rowIdx and colIdx to data
  rowDef <- Bauhaus.getIndexTable(data[,rowID,with=FALSE])
  colnames(rowDef) <- c(rowID,'rowIdx')
  data<-data[rowDef,nomatch=0,on=rowID]
  colDef <- Bauhaus.getIndexTable(data[,colID,with=FALSE])
  colnames(colDef) <- c(colID,'colIdx')
  data <- data[colDef,nomatch=0,on=colID]
  return(data)
}

Bauhaus.as.matrix <- function(data,rowID=colnames(data)[1],colID=colnames(data)[2]) {
  # data.table hell: on=c(x='a',y='b') joining with different colum names is broken
  # hence need to assign to colnames
  newColIdx = 1+dim(data)[2]
  rowDef <- Bauhaus.getIndexTable(data[,rowID,with=FALSE])
  colnames(rowDef) <- c(rowID,'rowIdx')
  dataPlus <- data[rowDef,nomatch=0,on=rowID] #[,newColIdx,with=FALSE]
  colDef <- Bauhaus.getIndexTable(dataPlus[,colID,with=FALSE])
  colnames(colDef) <- c(colID,'colIdx')
  dataPlus <- dataPlus[colDef,nomatch=0,on=colID] #[,newColIdx,with=FALSE]
  size <- c(nrow(rowDef),nrow(colDef))
  # data.table hell: data$ranking/data[,ranking] is a numeric array, but tt[,3] is a data.table
  # that sparseMatrix doesn't support data.table. Hence the following $ranking ($idx is ok as
  # it is created locally
  sm <- Matrix::sparseMatrix(i=dataPlus$rowIdx,j=dataPlus$colIdx,x=dataPlus$ranking,dims=size)
  rownames(sm) <- as.matrix(rowDef)[,1]
  colnames(sm) <- as.matrix(colDef)[,1]
  return(sm)
}

Bauhaus.matrix2transformed <- function(mat,cols) {
  # This function convert the data matrix to a data.table
  transformed <- as.data.table(data.table::melt(as.matrix(mat)))
  colnames(transformed) <- cols
  transformed <- transformed[get(cols[3])!=0]
  transformed[,(cols[1]):=as.character(get(cols[1]))]
  transformed[,(cols[2]):=as.character(get(cols[2]))]
  return(transformed)
}

Bauhaus.getItemBought <- function(transformed,cols,thres=c(2,1),type=c("bought","not")) {
  # This function gets the bought items
  itemBought <- unique(transformed[,mget(c(cols[1:2],"ranking"))])
  itemBought <- if (type=="bought") {
    # get the items that each customer bought
    itemBought[ranking>=thres,mget(cols[1:2])]
  } else {
    # get the items that each customer not bought
    itemBought[ranking<=thres,mget(cols[1:2])]
  }
  setkeyv(itemBought,cols[1:2])
  return(itemBought)
}

Bauhaus.getTopItem <- function(predTable,transformed,cols,type=c("bought","not"),n.recom=5) {
  # This function makes recommendations on the bought items
  setkeyv(predTable,cols[1:2])
  itemBoughtOrNot <- if(type=="bought") {
    Bauhaus.getItemBought(transformed,cols,2,"bought")
  } else {
    Bauhaus.getItemBought(transformed,cols,1,"not")
  }
  # Merge to select the prediction values on the bought items
  predTableWithItemBoughtOrNot <- merge(predTable,itemBoughtOrNot)
  # Exclude the prediction value of 1
  predTableWithItemBoughtOrNot <- predTableWithItemBoughtOrNot[Prediction>1,]
  # select the top <n.recom> item for recommendations
  predTableWithItemBoughtOrNot <- predTableWithItemBoughtOrNot[,rank:=frank(-Prediction),by=get(cols[1])]
  predTableWithItemBoughtOrNot <- predTableWithItemBoughtOrNot[rank<=n.recom]
  return(predTableWithItemBoughtOrNot[,mget(cols[1:2])])
}

Bauhaus.pred2List <- function(predMatrix,dataMatrix,cols) {
  # This function generates recommendation list from the prediction matrix
  predTable <- data.table::as.data.table(data.table::melt(predMatrix, na.rm = TRUE))
  colnames(predTable) <- c(cols[1:2], "Prediction")
  predTable[,(cols[1]):=as.character(get(cols[1]))]
  predTable[,(cols[2]):=as.character(get(cols[2]))]
  transformed <- Bauhaus.matrix2transformed(dataMatrix,c(cols[1:2],"ranking"))
  # Get the bought item for recommendations
  predList <- Bauhaus.getTopItem(predTable,transformed,cols,type="bought")
  # Get the not bought item for recommendations
  predList <- rbind(predList,Bauhaus.getTopItem(predTable,transformed,cols,type="not"))
  return(predList)
}

Bauhaus.getMemberGender <- function(txnTreated) {
  # This function gets the member gender from the transaction record
  memberGender <- unique(txnTreated[,.(MemberID,Gender)])
  memberGender$MemberID <- as.character(memberGender$MemberID)
  setkey(memberGender,MemberID)
  return(memberGender)
}

Bauhaus.getDerivedCategory <- function(txnTreated) {
  # This function gets all derived categories from the transaction record
  derivedCategory <- unique(txnTreated[,.(derivedCategory,Category,Gender)])
  setkey(derivedCategory,Category,Gender)
  return(derivedCategory)
}

Bauhaus.getLatestProduct <- function(txnTreated,cumQty.min=20,period=3) {
  # This function gets the latest product in the past <period> months
  # Remark: This function can be replaced to read a list of latest products
  #         for recommendations with promotion factors (i.e.: a weight for
  #         the likelihood of a particular item to be recommended)
  txnTreated$Timestamp <- as.Date(txnTreated$Timestamp)
  latestDate <- max(txnTreated$Timestamp)
  beginDate <- latestDate - period*30
  # Extract transaction record on or after <beginDate>
  txnTreatedLatest <- txnTreated[Timestamp>=beginDate, 
                                 .(ProductID,ProductName_x,Quantity,
                                   Gender,ProductName_y,DepartName,
                                   derivedCategory,Category)]
  # Compute the cumulative quantity of products in the period
  txnTreatedLatest <- txnTreatedLatest[,.(CumQty=sum(Quantity)), 
                                       by=c("Gender","derivedCategory","ProductName_y")]
  # Get the top 2 best sell products for each derivedCategory and gender
  txnTreatedLatest <- (txnTreatedLatest %>%
                          group_by(Gender,derivedCategory) %>%
                          top_n(n = 2, wt = CumQty))
  txnTreatedLatest <- as.data.table(txnTreatedLatest)
  # Get only the products staifying the conditions
  txnTreatedLatest <- txnTreatedLatest[,.SD[CumQty==max(CumQty)|CumQty>=cumQty.min],
                                       by=c("derivedCategory","Gender")]
  # The CumQty is consider as the promotion factor
  setnames(txnTreatedLatest,"CumQty","PromotionFactor")
  setnames(txnTreatedLatest,"ProductName_y","ProductName")
  setkey(txnTreatedLatest,Gender,derivedCategory)
  return(txnTreatedLatest)
}

Bauhaus.predList2RecomList <- function(predList,memberGender,derivedCategory) {
  # This function converts the prediction list (from 'Bauhaus.pred2List') to
  # RecomList, which consists of the member gender, all possible derivedCategory of 
  # the recommended category
  # Merge gender to predList
  setkey(predList,MemberID)
  RecomCat <- predList[memberGender,nomatch=0]
  # Merge derivedCatergory to RecomCat
  setkey(RecomCat,Category,Gender)
  RecomCat <- RecomCat[derivedCategory,nomatch=0,allow.cartesian=TRUE]
  numDistinctCategory <- function(x){
    # This function counts the distinct derived categories for each member
    return(length(x))
  }
  RecomCat <- RecomCat[,numDistCat:=numDistinctCategory(Gender),by=c("MemberID","derivedCategory")]
  RecomCat <- unique(RecomCat[,.(MemberID,Gender,derivedCategory,Category,numDistCat)])
  # String concatenate the catergory
  RecomCat <- unique(RecomCat[,Category:=toString(Category),by=.(MemberID,Gender,derivedCategory,numDistCat)])
  setkey(RecomCat,Gender,derivedCategory)
  return(RecomCat)
}

Bauhaus.category2Product <- function(RecomCat,latestProducts,
                                     top.promoFactor=2,select.n=1) {
  # This function converts the recommended category to products
  # Get the latest products
  if(!("PromotionFactor" %in% colnames(latestProducts))) {
    # Check if the table contains promotion factor, otherwise
    # set to 1, meaning all products are equal weighted
    latestProducts[,PromotionFactor:=1]
  }
  setkey(RecomCat,Gender,derivedCategory)
  # Merge the tables for product recommendations
  RecomCat <- RecomCat[latestProducts,nomatch=0]
  # Get the products with higher promotion factor for recommendations
  RecomCat <- (RecomCat %>%
                 group_by(MemberID,Category,numDistCat) %>%
                 top_n(n=top.promoFactor,wt=PromotionFactor))
  RecomCat <- as.data.table(RecomCat)
  # Randomly select <select.n> products for recommendations
  RecomItems <- RecomCat[,.SD[sample(.N,select.n)],by=c("MemberID","Category",
                                                        "numDistCat")]
  return(RecomItems[,.(MemberID,ProductName)])
}

Bauhaus.distance2List <- function(distMatrix) {
  # This function generates recommendation list from the distance matrix
  distTable <- as.data.table(melt(distMatrix,na.rm=TRUE))
  colnames(distTable) <- c("from","to","distance")
  # the distance = 0 means the pair of items are closely related
  # for example "kids" and "cookies" happen at the same time
  distTable <- distTable[distance!=0,]
  # find the most similar items for a given item to construte the matchTable
  distTable <- distTable[,rank:=frank(distance),by="from"]
  distTable <- distTable[rank<=1.5,.(from,to)]
  distTable[,from:=as.character(from)]
  distTable[,to:=as.character(to)]
  setkey(distTable,from)
  return(distTable)
}

Bauhaus.distList2predList <- function(distList,dataMatrix,cols,select.n=10) {
  # Get the top ranking categories
  transformed <- Bauhaus.matrix2transformed(dataMatrix,c(cols[1:2],"ranking"))
  setkeyv(transformed,cols[2])
  predList <- transformed[distList,allow.cartesian=TRUE]
  # The function "group_by_at" requires dplyr with version >= 0.7.1
  predList <- as.data.table((predList %>%
                 group_by_at(cols[1]) %>%
                 top_n(n=select.n,wt=ranking)))
  predList <- unique(predList[,c(cols[1],"to"),with=FALSE])
  setnames(predList,"to",cols[2])
  predList[,(cols[1]):=as.character(get(cols[1]))]
  predList[,(cols[2]):=as.character(get(cols[2]))]
  return(predList)
}

##########################################################################################
# This part is for Customer Lifetime Value
##########################################################################################

Bauhaus.txn2elog <- function(txnData, method = "day", max.length = 1){
  return(txn2elog(txnData, method, max.length))
  # # some customers' intertransaction time are just 10 min or 1 hour
  # # maybe there two transations can be aggregate to be one transations
  # # arguments: 
  # #        txnData is data.table 
  # #        method is the aggregate type:
  # # for example: method = "day", max.length = 1 then the transactions within one day is one txn for each customer
  # # method = "second", max.length = 7200 then the intertransation time which two transations will be aggregated to one txn
  # if (method == "day"){
  #   txnData$Timestamp <- as.Date(txnData$Timestamp)
  # } else if (method == "second"){
  #   txnData <- txnData[order(MemberID,Timestamp)]
  #   txnData <- txnData[,first := min(Timestamp), by = .(MemberID)]
  #   # TODO: change the iterator
  #   for(i in 1:100000){
  #     txnData <- txnData[,t := difftime(Timestamp,first,units = "secs")]
  #     txnData <- txnData[,itt := c(0,diff(t)),by = .(MemberID)]
  #     rownumber <- which(txnData$itt < max.length & txnData$itt > 0 )
  #     txnData[rownumber-1,]$Timestamp <- txnData[rownumber,]$Timestamp
  #     txnData$t <- txnData$itt <- NULL
  #     if(length(rownumber)==0){
  #       break
  #     }
  #   }  
  # } 
  # txnData <- txnData[,.(sales = sum(TxnAmt)), by = .(MemberID,Timestamp)]
  # names(txnData) <- c("cust","date","sales")
  # return(txnData)
}

Bauhaus.getElog <- function(txn, units = "days") {
  # some problems for Bauhaus data:
  # (1) the col "TxnTotalAmt" is not equal to sum of col "TotalPrice"
  # (2) one OrderID is related to two MemberID
  #     total six txn with type 2 problem
  
  # TODO: include the following to the data cleaning part, check coverage?
  # aggregated the total quantity for each transaction 
  if ("NetPrice" %in% names(txn)) {
    txnTreated <- txn[,.(TxnAmt = sum(NetPrice)), by = .(OrderID,MemberID,Timestamp)]
  } else {
    txnTreated <- txn[,.(TxnAmt = sum(TotalPrice)), by = .(OrderID,MemberID,Timestamp)]
  }
  return(getElog(txnTreated, units))
  # # remove the duplicated OrderID in the txnData
  # duplicatedOrderID <- txnData[duplicated(txnData$OrderID),]$OrderID
  # txnData <- txnData[!OrderID %in% duplicatedOrderID,]
  # 
  # # There are some transactions with the same Timestamp and MemberID but different OrderID and TxnAmt
  # # so remove those transacions
  # txnData[duplicated(txnData[,.(MemberID,Timestamp)])|
  #           duplicated(txnData[nrow(txnData):1,.(MemberID,Timestamp)])[nrow(txnData):1],]
  # txnData <- txnData[!duplicated(txnData[,.(MemberID,Timestamp)])&
  #                      !duplicated(txnData[nrow(txnData):1,.(MemberID,Timestamp)])[nrow(txnData):1],]
  # 
  # # aggregate the transactions within one day
  # elog <- Bauhaus.txn2elog(txnData, method = "day", max.length = 1)
  # return(elog)
}

Bauhaus.getHoldoutDate <- function(txnData, p = 0.2) {
  return(getHoldoutDate(txnData, p))
  # # to get the first day of the holdout period
  # maxDate <- max(txnData$date)
  # minDate <- min(txnData$date)
  # holdoutDate <- maxDate - (maxDate - minDate) * p
  # # holdout peried will be the holdoutDate to the maxDate of the transations
  # return(holdoutDate)
}

Bauhaus.getAcquiredDate <- function(txnData, p = 0.2){
  return(getAcquiredDate(txnData, p))
  # # get the last day of acquire period
  # # the customers in the acquired period will be selected to train the model
  # maxDate <- max(txnData$date)
  # minDate <- min(txnData$date)
  # acquiredDate <- minDate + (maxDate - minDate) * p
  # # acquired period will be the minDate of the transations to the acquiredDate
  # return(acquiredDate)
}