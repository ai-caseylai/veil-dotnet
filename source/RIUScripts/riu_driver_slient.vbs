Set WshShell = CreateObject("WScript.Shell")
WshShell.CurrentDirectory = "E:\RIUScripts"
WshShell.Run chr(34) & "E:\RIUScripts\riu_driver.bat" & chr(34), 0
Set WshShell = Nothing
