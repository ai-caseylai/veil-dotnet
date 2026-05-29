# current_date is the current date
$current_date = get-date -format "yyyyMMdd"
# Obtain the data from SQL
sqlcmd -S 192.168.28.22 -d SVA_7Fans_Reporting -U "v-patrick.yuen" -P "998@abcdef#" -o "E:\RIUData\7Fans\txn\107_$current_date.csv.tmp" -h -1 -u -W -w 9999 -s"," -i E:\RIUScripts\SQL\107_daily_txn.sql
Get-Content "E:\RIUData\7Fans\txn\107_$current_date.csv.tmp" | Set-Content -Encoding utf8 "E:\RIUData\7Fans\txn\107_$current_date.csv"
# The xz is downloaded from https://tukaani.org/xz/ and placed at E:\xz. !!! Add the path to the Environment variables. 
# This is used to compress the csv in xz format.
xz -z "E:\RIUData\7Fans\txn\107_$current_date.csv"
Remove-Item "E:\RIUData\7Fans\txn\107_$current_date.csv.tmp"