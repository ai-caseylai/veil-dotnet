# The current_date is the current date
$current_date = get-date -format "yyyyMMdd"
# This is the path for the RFM analysis
cd E:\RIUScripts\RFM
# The line below is to run the RFM analysis or customer segmentation, 
# path:  The link to the python data processor, default is "http://127.0.0.1:8888"
# bu: The company code, e.g.: 106 is Bauhaus, 107 is sevenFans
# out: The analysis result in rds format
Rscript RFM-driver.R --path "http://127.0.0.1:8888" --bu 107 --out "../../RIURDS/RFM_107_$current_date.rds" > log/RFM_107_$current_date.log 2>&1
# The line below is to run the Customer Segment Transition, the output is in a matrix.
# path:  The link to the python data processor, default is "http://127.0.0.1:8888"
# bu: The company code, e.g.: 106 is Bauhaus, 107 is sevenFans
# out: The analysis result in rds format
Rscript RFM-MC-driver.R --path "http://127.0.0.1:8888" --bu 107 --out "../../RIURDS/RFM_MC_107_$current_date.rds" > log/RFM_MC_107_$current_date.log 2>&1
