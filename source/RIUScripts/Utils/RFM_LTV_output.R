# This script is to generate the result from rds to csv 
library(data.table)
library(optparse)
source("../RFM/rfmMC.R")
source("../LTV/MonetaryValue.R")

current <- Sys.time() %m-% days(1)
end <- format(current, "%Y%m%d")
optionList <- list(make_option(c('--rdspath'), default = ""),
                   make_option(c('--bu'), default = 106),
                   make_option(c('--date'), default = end),
                   make_option(c('--outpath')))
parser <- OptionParser(usage = "%prog [options]", option_list = optionList);
cmdOptions <- parse_args(parser)

# This is to generate the RFM and LTV results
filename_bu_date <- paste(cmdOptions$bu, cmdOptions$date, sep = "_")
rfm_list <- readRDS(paste0(cmdOptions$rdspath, "/RFM_", filename_bu_date, ".rds"))
ltv_list <- readRDS(paste0(cmdOptions$rdspath, "/LTV_", filename_bu_date, ".rds"))
rfm_ltv_output <- merge(rfm_list$rfmData, rfm_list$rfmScore, by = "CustomerID")
rfm_ltv_output <- merge(rfm_ltv_output, rfm_list$rfmSegment, by = "CustomerID")
rfm_ltv_output <- merge(rfm_ltv_output, ltv_list$customerSummary, by = "CustomerID")
rfm_ltv_output$lifetimeValue <- format(rfm_ltv_output$lifetimeValue, scientific = F, digits = 8)
rfm_ltv_output$pAlive <- format(rfm_ltv_output$pAlive, scientific = F, digits = 8)

out_filename <- paste0(cmdOptions$outpath, "/RFM_LTV_", filename_bu_date, ".csv")
fwrite(rfm_ltv_output, out_filename)

# This is to generate the transition probabilities
rfm_mc_list <- readRDS(paste0(cmdOptions$rdspath, "/RFM_MC_", filename_bu_date, ".rds"))
rfm_mc_mat <- rfm_mc_list$transProb

rfm_mc_df <- melt(rfm_mc_mat)
rfm_mc_df$value <- format(rfm_mc_df$value, scientific = F, digits = 8)
colnames(rfm_mc_df) <- c("From", "To", "Probability")

out_filename <- paste0(cmdOptions$outpath, "/RFM_MC_", filename_bu_date, ".csv")
fwrite(rfm_mc_df, out_filename)

# This is to generate the RFM segments prediction
initProp <- mcSeq2Prop(rfm_mc_list$mcSeq)
nCustomer <- rfm_mc_list$mcSeq[Time == max(Time), .N]
predictedSegmentMovement <- predictSegmentMovement(initProp, rfm_mc_list$transProb, 
                                                   nCustomer, 12)
colnames(predictedSegmentMovement) <- c("Month", "Segment", "Predicted")

out_filename <- paste0(cmdOptions$outpath, "/RFM_MC_Segment_", filename_bu_date, ".csv")
fwrite(predictedSegmentMovement, out_filename)

# This is to generate the average spending per txn for each segment dataset for the graph
expected.MonetaryValue <- expectation.monetaryValue.per.txn(ltv_list$cbs_full, ltv_list$monetaryValue.params)
monetary_value <- merge(expected.MonetaryValue, rfm_list$rfmSegment, by.x = "cust", by.y = "CustomerID")
avg_spending_per_txn <- monetary_value[, .(Actual = mean(m.x), Predicted = mean(expected.avg.txn.value)), 
                                       by = "Segment"]
out_filename <- paste0(cmdOptions$outpath, "/Avg_Spending_per_Txn_", filename_bu_date, ".csv")
fwrite(avg_spending_per_txn, out_filename)