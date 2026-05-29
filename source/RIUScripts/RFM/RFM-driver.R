#!/usr/bin/env Rscript
# usage: [script] --path "http://127.0.0.1:8888" --bu 106

library(methods)
library(data.table)
library(lubridate)
library(optparse)
source("../RFM/findRFM.R")

current <- Sys.time() %m-% days(1)
end <- format(current, "%Y%m%d")
begin <- format(current %m-% years(1), "%Y%m%d")
optionList <- list(make_option(c('--path'), default = ""),
                   make_option(c('--bu'), default = 106),
                   make_option(c('--begin'), default = begin),
                   make_option(c('--end'), default = end),
                   make_option(c('--out')),
                   make_option(c('--type'), default = "rds"))
parser <- OptionParser(usage = "%prog [options]", option_list=optionList);
cmdOptions <- parse_args(parser)

switch(as.character(cmdOptions$bu),
  "106" = {
    source("../Utils/bauhaus.R")
  },
  "107" = {
    source("../Utils/sevenFans.R")
  }
)

rfmData <- readRFMfromURL(cmdOptions$path, cmdOptions$bu, cmdOptions$begin, 
                          cmdOptions$end)
rfmScore <- rfmCompute(rfmData)
rfmSegment <- rfmSegmentation(rfmScore)

if (is.null(cmdOptions$out)) {
  cmdOptions$out <- paste(paste("RFM", cmdOptions$bu, end, sep = "_"), sep = "")
}

if (cmdOptions$type == "csv") {
  if (!grepl(".csv", cmdOptions$out)) {
    cmdOptions$out <- paste(cmdOptions$out, ".csv", sep = "")
  }
  output <- rfmData[,.(CustomerID, DaySinceLastTxn, NoOfTxn, MeanMoneyValue, TotalSpending)]
  output <- output[rfmScore[,.(CustomerID, RecencyScore, FrequencyScore, MonetaryScore)]]
  output <- output[rfmSegment]
  setnames(output, c("MeanMoneyValue"), c("AverageSpending"))
  
  cmdOptions$out <- paste(cmdOptions$out, ".csv", sep = "")
  fwrite(output, cmdOptions$out)
} else {
  if (!grepl(".rds", cmdOptions$out)) {
    cmdOptions$out <- paste(cmdOptions$out, ".rds", sep = "")
  }
  saveRDS(list(rfmData = rfmData, rfmScore = rfmScore, rfmSegment = rfmSegment), 
          file = cmdOptions$out, compress = "xz")
}
