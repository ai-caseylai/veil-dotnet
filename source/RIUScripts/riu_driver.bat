REM This is the riu driver
@echo on

cd E:/RIUScripts

REM Start the data processor
call activate riu_data_processor
cd E:/RIUScripts/DataProcessor
start /I /B "" python ./map_reduce-driver.py --datapath ../../RIUData
call deactivate

cd E:/RIUScripts/Shiny
start /I /B "" Rscript ./runShinyApp.R

REM This is the R Shiny server with Chiness translation
cd E:/RIUScripts/Shiny1
start /I /B "" Rscript ./runShinyApp.R --port 16478
