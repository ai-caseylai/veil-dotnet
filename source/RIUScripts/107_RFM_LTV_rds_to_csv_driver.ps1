# This script is to generate the csv from the rds file
# The current_date is the current date
$current_date = get-date -format "yyyyMMdd"
# This is the path for the R script
cd E:\RIUScripts\Utils
# The line below is to run the LTV analysis, to output LTV analysis result in table.
# rdspath:  The path that the rds files store
# bu: The company code, e.g.: 106 is Bauhaus, 107 is sevenFans
# date: The date of the rds file to be transformed
# out: The prefix of the csv filename
Rscript RFM_LTV_output.R --rdspath "E:/RIURDS" --bu 107 --date "$current_date" --out "E:/RIURDS/" > log/RFM_LTV_rds_to_csv_107_$current_date.log 2>&1
