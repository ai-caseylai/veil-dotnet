#!/usr/bin/env Rscript
# usage: [script] --path "http://127.0.0.1:8888" --bu 106

library(lubridate)
library(optparse)
source("../Utils/master.R")
source("../LTV/BTYDmodels.R")
source("../LTV/MonetaryValue.R")
source("../LTV/findLTV.R")

current <- Sys.time() %m-% days(1)
end <- format(current, "%Y%m%d")
begin <- format(current %m-% months(15), "%Y%m%d")

optionList <- list(make_option(c('--path'), default = ""),
                   make_option(c('--bu'), default = 106),
                   make_option(c('--begin'), default = begin),
                   make_option(c('--end'), default = end),
                   make_option(c('--method'), default = "mbgnbd"),
                   make_option(c('--discount'), default = 0.3),
                   make_option(c('--margin'), default = 0.3),
                   make_option(c("--nperiod"), default = 52),
                   make_option(c("--params")),
                   make_option(c('--out')),
                   make_option(c('--type'), default = "rds"))
parser <- OptionParser(usage = "%prog [options]", option_list = optionList);
cmdOptions <- parse_args(parser)

switch(as.character(cmdOptions$bu),
       "106" = {
         source("../Utils/bauhaus.R")
         local.getElog <- Bauhaus.getElog
         readtxn <- Bauhaus.readtxn
       },
       "107" = {
         source("../Utils/sevenFans.R")
         local.getElog <- sevenFans.getElog
         readtxn <- sevenFans.readtxn
       }
)

txnData <- readtxn(file = cmdOptions$path, begin = cmdOptions$begin, 
                   end = cmdOptions$end, 
                   select = c("Timestamp", "OrderID", "MemberID", "NetPrice",
                              "Quantity"))
txnData$Timestamp <- as.Date(txnData$Timestamp)
elog <- local.getElog(txnData)

cbs_selected <- getSelectedCBS(elog)
if (is.null(cmdOptions$params)) {
  model.params <- fit.BTYD(cbs_selected, elog[cust %in% cbs_selected$cust], 
                           method = cmdOptions$method)
} else {
  model.params <- readRDS(cmdOptions$params)
}
monetaryValue.params <- fit.MonetaryValue(elog[cust %in% cbs_selected$cust & 
                                                 date <= getHoldoutDate(elog)])

cbs_full <- getCBS(elog, 0)
ltv <- findLTV(cbs_full, model.params, monetaryValue.params, 
               method = cmdOptions$method, nperiod = cmdOptions$nperiod, 
               d = cmdOptions$discount, margin = cmdOptions$margin)
prob.alive <- findPAlive(cbs_full, model.params, cmdOptions$method)

output <- ltv
output <- output[prob.alive]

if (is.null(cmdOptions$out)) {
  cmdOptions$out <- paste("LTV", cmdOptions$bu, end, sep = "_")
}

if (cmdOptions$type == "csv") {
  if (!grepl(".csv", cmdOptions$out)) {
    cmdOptions$out <- paste(cmdOptions$out, ".csv", sep = "")
  }
  setnames(output, c("lifetimeValue", "pAlive"), 
           c("Customer Lifetime Value", "Probability of Alive"))
  fwrite(output, cmdOptions$out)
} else {
  if (!grepl(".rds", cmdOptions$out)) {
    cmdOptions$out <- paste(cmdOptions$out, ".rds", sep = "")
  }
  saveRDS(list(customerSummary = output, 
               monetary_params = monetaryValue.params,
               cbs_full = cbs_full), 
          file = cmdOptions$out, compress = "xz")
}
