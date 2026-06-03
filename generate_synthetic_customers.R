#!/usr/bin/env Rscript
# =============================================================================
# Generate 1000 synthetic customers for Veil CRM what-if analysis
# Integrates into the existing RDS pipeline so Shiny dashboard can load them
# =============================================================================
library(data.table)
library(lubridate)

set.seed(20260603)
REFERENCE_DATE <- as.Date("2026-06-03")

# ---- Load reference data ----
existing_txn <- fread("RIUData/7Fans/txn/sample_txn.csv")
existing_products <- unique(existing_txn[, .(ProductID, ProdName1, Category, DepartCode, NetPrice)])
gender_dist <- existing_txn[, .N, by = Gender]
gender_dist[, prob := N / sum(N)]

cat("=== Reference Data ===\n")
cat("Products:", nrow(existing_products), "| Txns:", nrow(existing_txn), 
    "| Customers:", uniqueN(existing_txn$MemberID), "\n\n")

# ---- Parameters (matching user specs) ----
N_CUSTOMERS    <- 1000
FREQ_MIN_DAYS  <- 3     # 2x per week
FREQ_MAX_DAYS  <- 28    # 1x per 4 weeks
VALUE_MIN      <- 50
VALUE_MAX      <- 800
ITEMS_MIN      <- 1
ITEMS_MAX      <- 6

# Match existing recency distribution: 17-504 days, mean ~176, median ~159
# Use a log-normal approximation calibrated to existing data
RECENCY_MEANLOG <- 4.8
RECENCY_SDLOG   <- 0.8

# ---- Generate one customer ----
gen_customer <- function(id) {
  interval  <- runif(1, FREQ_MIN_DAYS, FREQ_MAX_DAYS)
  # Recency (days since last purchase) — match existing distribution
  recency   <- min(500, max(10, round(rlnorm(1, RECENCY_MEANLOG, RECENCY_SDLOG))))
  # Lifespan from first to last order
  lifespan  <- max(14, round(runif(1, 30, 400)))
  n_orders  <- max(1, round(lifespan / interval))
  # Last order date = today - recency
  last_day  <- REFERENCE_DATE - recency
  first_day <- last_day - lifespan
  gender    <- sample(gender_dist$Gender, 1, prob = gender_dist$prob)
  
  rows <- vector("list", n_orders * ITEMS_MAX)  # pre-allocate
  ri <- 1
  for (o in seq_len(n_orders)) {
    odate    <- as.Date(first_day + round((o-1)*interval + runif(1, -2, 2)))
    if (odate > REFERENCE_DATE) odate <- REFERENCE_DATE - 1
    oid      <- sprintf("S%07d%03d", id, o)
    n_items  <- sample(ITEMS_MIN:ITEMS_MAX, 1)
    target   <- runif(1, VALUE_MIN, VALUE_MAX)
    picks    <- sample(nrow(existing_products), n_items, replace = TRUE)
    
    # Calculate raw totals first, then scale to hit target
    raw_items <- lapply(seq_len(n_items), function(i) {
      p <- existing_products[picks[i]]
      qty <- sample(1:3, 1)
      price <- round(p$NetPrice * runif(1, 0.8, 1.3), 2)
      list(p = p, qty = qty, price = price, subtotal = price * qty)
    })
    raw_total <- sum(sapply(raw_items, `[[`, "subtotal"))
    scale_factor <- target / raw_total
    
    for (i in seq_len(n_items)) {
      ri_item <- raw_items[[i]]
      final_price <- round(ri_item$price * scale_factor, 2)
      card <- sample(1e9:(1e10-1), 1)
      
      rows[[ri]] <- list(CardNumber = card, MemberID = sprintf("S%06d", id),
                         OrderID = oid, Timestamp = format(odate, "%Y-%m-%d 00:00:00"),
                         ProductID = ri_item$p$ProductID, ProdName1 = ri_item$p$ProdName1,
                         Category = ri_item$p$Category, DepartCode = ri_item$p$DepartCode,
                         NetPrice = final_price, Quantity = ri_item$qty, Gender = gender)
      ri <- ri + 1
    }
  }
  rbindlist(rows[1:(ri-1)])
}

# ---- Generate all customers ----
cat("Generating", N_CUSTOMERS, "customers...\n")
t0 <- Sys.time()
all <- rbindlist(lapply(1000:(1000+N_CUSTOMERS-1), gen_customer))
cat(sprintf("Done in %.1f sec\n", difftime(Sys.time(), t0, units = "secs")))

# ---- Summary ----
cat("\n=== Synthetic Data Summary ===\n")
cat("Rows:", nrow(all), "| Customers:", uniqueN(all$MemberID), 
    "| Orders:", uniqueN(all$OrderID), "\n")
items_po <- all[, .(items = .N), by = OrderID]
cat("Items/order:  ", sep=""); print(summary(items_po$items))
vals_po <- all[, .(total = sum(NetPrice * Quantity)), by = OrderID]
cat("Value/order($):", sep=""); print(summary(vals_po$total))
ords_pc <- all[, .(orders = uniqueN(OrderID)), by = MemberID]
cat("Orders/cust:  ", sep=""); print(summary(ords_pc$orders))

# ---- Save transaction CSV ----
dir.create("RIUData/7Fans/txn", showWarnings = FALSE, recursive = TRUE)
fwrite(all, "RIUData/7Fans/txn/synthetic_1000_txn.csv")
cat("\nSaved: RIUData/7Fans/txn/synthetic_1000_txn.csv\n")

# =============================================================================
# RFM PIPELINE — need to run from Shiny dir for relative source() paths
# =============================================================================
cat("\n=== RFM Pipeline ===\n")
orig_wd <- getwd()
setwd("source/RIUScripts/Shiny")
source("../Utils/master.R")
source("../RFM/findRFM.R")
source("../RFM/rfmMC.R")
source("../LTV/findLTV.R")
setwd(orig_wd)

# Prepare data for RFM: add TotalPrice column, drop NetPrice so getRFMData uses TotalPrice
txn_for_rfm <- copy(all)
txn_for_rfm[, TotalPrice := NetPrice * Quantity]
txn_for_rfm[, NetPrice := NULL]  # force getRFMData to use TotalPrice
txn_for_rfm$Timestamp <- as.POSIXct(txn_for_rfm$Timestamp)

# getRFMData expects: OrderID, MemberID, Timestamp, TotalPrice
# sumTotal=TRUE is CRITICAL: it aggregates line items per order before dedup
rfmData <- getRFMData(txn_for_rfm, sumTotal = TRUE)
cat("rfmData:", nrow(rfmData), "customers\n")

rfmScore <- rfmCompute(rfmData)
cat("rfmScore:", nrow(rfmScore), "customers\n")

rfmSegment <- rfmSegmentation(rfmScore)
cat("rfmSegment:", nrow(rfmSegment), "customers\n")

cat("\nSegment distribution:\n")
print(sort(table(rfmSegment$Segment), decreasing = TRUE))

# ---- Save RFM RDS ----
dir.create("source/RIURDS", showWarnings = FALSE, recursive = TRUE)
rfmlist <- list(rfmData = rfmData, rfmScore = rfmScore, rfmSegment = rfmSegment)
saveRDS(rfmlist, "source/RIURDS/RFM_107_synthetic1000.rds")
cat("Saved: source/RIURDS/RFM_107_synthetic1000.rds\n")

# =============================================================================
# MARKOV CHAIN (Transition Probabilities) — need multi-period data
# =============================================================================
cat("\n=== Markov Chain ===\n")

# Split transactions into periods for transition analysis
# Period 1: transactions before the median date
# Period 2: transactions after the median date
txn_dates <- as.Date(txn_for_rfm$Timestamp)
split_date <- median(txn_dates)
cat("Split date:", format(split_date, "%Y-%m-%d"), "\n")

txn_p1 <- txn_for_rfm[as.Date(Timestamp) <= split_date]
txn_p2 <- txn_for_rfm[as.Date(Timestamp) > split_date]
cat("Period 1 rows:", nrow(txn_p1), "| Period 2 rows:", nrow(txn_p2), "\n")

seg_p1 <- rfmSegmentation(rfmCompute(getRFMData(txn_p1, sumTotal = TRUE)))
seg_p2 <- rfmSegmentation(rfmCompute(getRFMData(txn_p2, sumTotal = TRUE)))
cat("Period 1 segments:", nrow(seg_p1), "| Period 2 segments:", nrow(seg_p2), "\n")

# Build the mcSeq and transition matrix
rfmSegmentList <- list(seg_p1, seg_p2)
mcSeq <- rfmSegment2mcSeq(rfmSegmentList)
mcCounts <- mcSeq2Counts(mcSeq)
transProb <- mcCounts2Prob(mcCounts)

cat("mcSeq periods:", max(mcSeq$Time), "\n")
cat("mcCounts periods:", length(mcCounts), "\n")
cat("transProb dim:", paste(dim(transProb), collapse = "x"), "\n")

mclist <- list(mcSeq = mcSeq, mcCounts = mcCounts, transProb = transProb)
saveRDS(mclist, "source/RIURDS/RFM_MC_107_synthetic1000.rds")
cat("Saved: source/RIURDS/RFM_MC_107_synthetic1000.rds\n")

# =============================================================================
# LTV (simplified — based on TotalSpending × random factor)
# =============================================================================
cat("\n=== LTV (simplified) ===\n")

ltv_dt <- rfmData[, .(
  CustomerID = CustomerID,
  lifetimeValue = TotalSpending * runif(.N, 0.2, 0.6),
  pAlive = round(rbeta(.N, 8, 2), 6)
)]
setkey(ltv_dt, CustomerID)
cat("LTV rows:", nrow(ltv_dt), "\n")
cat("lifetimeValue: mean =", round(mean(ltv_dt$lifetimeValue), 2), 
    ", median =", round(median(ltv_dt$lifetimeValue), 2), "\n")

ltvlist <- list(customerSummary = ltv_dt)
saveRDS(ltvlist, "source/RIURDS/LTV_107_synthetic1000.rds")
cat("Saved: source/RIURDS/LTV_107_synthetic1000.rds\n")

# =============================================================================
# Symlink to today's date so Shiny findRDS picks them up
# =============================================================================
date_str <- format(Sys.Date(), "%Y%m%d")
for (prefix in c("RFM_", "RFM_MC_", "LTV_")) {
  target <- paste0("source/RIURDS/", prefix, "107_synthetic1000.rds")
  link   <- paste0("source/RIURDS/", prefix, "107_", date_str, ".rds")
  suppressWarnings(file.remove(link))
  file.symlink(basename(target), link)
  cat("Linked:", link, "->", basename(target), "\n")
}

cat("\n========================================\n")
cat("  DONE! 1000 synthetic customers ready\n")
cat("  Refresh https://veil.techforliving.net/?bu=107\n")
cat("========================================\n")
