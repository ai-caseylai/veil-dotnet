# The current_date is the current date
$current_date = get-date -format "yyyyMMdd"
# This is the path for the LTV (customer LifeTime Value) analysis
cd E:\RIUScripts\LTV
# The line below is to run the LTV analysis, to output LTV analysis result in table.
# path:  The link to the python data processor, default is "http://127.0.0.1:8888"
# bu: The company code, e.g.: 106 is Bauhaus, 107 is sevenFans
# out: The analysis result in rds format
Rscript LTV-driver.R --path "http://127.0.0.1:8888" --bu 107 --out "../../RIURDS/LTV_107_$current_date.rds" > log/LTV_107_$current_date.log 2>&1
