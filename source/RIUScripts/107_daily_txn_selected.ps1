for($day = 2; $day -lt 25; $day++)
{
	$current_date = ""
	if($day -lt 10)
	{
		$current_date = "2019010" + $day.ToString()
	}
	else
	{
		$current_date = "201901" + $day.ToString()
	}
	sqlcmd -S 192.168.28.22 -d SVA_7Fans_Reporting -U "v-patrick.yuen" -P "998@abcdef#" -o "E:\RIUData\7Fans\txn\107_$current_date.csv.tmp" -h -1 -u -W -w 9999 -s"," -i E:\RIUScripts\SQL\107_daily_txn_selected.sql -v txndate="$current_date"
	Get-Content "E:\RIUData\7Fans\txn\107_$current_date.csv.tmp" | Set-Content -Encoding utf8 "E:\RIUData\7Fans\txn\107_$current_date.csv"
	xz -z "E:\RIUData\7Fans\txn\107_$current_date.csv"
	Remove-Item "E:\RIUData\7Fans\txn\107_$current_date.csv.tmp"
}