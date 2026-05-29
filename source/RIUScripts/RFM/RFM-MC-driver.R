#!/usr/bin/env Rscript
# usage: [script] --path "http://127.0.0.1:8888" --bu 106

library(methods)
library(data.table)
library(lubridate)
library(optparse)
source("../RFM/findRFM.R")
source("../RFM/rfmMC.R")

current <- Sys.time() %m-% days(1)
end <- format(current, "%Y%m%d")
begin <- format(current %m-% months(18), "%Y%m%d")
optionList <- list(make_option(c('--path'), default = ""),
                   make_option(c('--bu'), default = 106),
                   make_option(c('--begin'), default = begin),
                   make_option(c('--end'), default = end),
                   make_option(c('--out')))
parser <- OptionParser(usage = "%prog [options]", option_list = optionList);
cmdOptions <- parse_args(parser)

switch(as.character(cmdOptions$bu),
  "106" = {
    source("../Utils/bauhaus.R")
    readtxn <- Bauhaus.readtxn
  },
  "107" = {
    source("../Utils/sevenFans.R")
    readtxn <- sevenFans.readtxn
  }
)

txnData <- readtxn(file = cmdOptions$path, begin = cmdOptions$begin, 
                   end = cmdOptions$end, 
                   select = c("Timestamp", "OrderID", "NetPrice", "MemberID"))
mcSeq <- rfmSegment2mcSeq(txn2rfmSegmentList(txnData))
mcCounts <- mcSeq2Counts(mcSeq, RFM_SEGMENT)
transProb <- mcCounts2Prob(mcCounts, stationary = TRUE, stateSpace = RFM_SEGMENT)

if (is.null(cmdOptions$out)) {
  cmdOptions$out <- paste(paste("RFM_MC", cmdOptions$bu, end, sep = "_"), sep = "")
}

if (!grepl(".rds", cmdOptions$out)) {
  cmdOptions$out <- paste(cmdOptions$out, ".rds", sep = "")
}
saveRDS(list(mcSeq = mcSeq, mcCounts = mcCounts, transProb = transProb), 
        file = cmdOptions$out, compress = "xz")
