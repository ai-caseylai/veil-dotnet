README
This is a rought readme for the demo.

Installation
1. Install the Anaconda Python from https://repo.anaconda.com/archive/Anaconda3-2018.12-Windows-x86_64.exe
2. Install the Microsoft R from https://mran.blob.core.windows.net/install/mro/3.5.1/microsoft-r-open-3.5.1.exe

Changing the path
1. Please change the path in the riu_driver_slient.vbs on line 2 and 3
2. Please change the path in the riu_driver.bat on line 4 and 8
3. Please add REM to comment out line 7 and 10 of riu_driver.bat

How to use it
1. Install the required python packages by executing pip install -r requirements.txt at the RIUScripts directory
2. Install the required R packages by executing Rscript -e 'install.packages(c("data.table","dplyr","lubridate","ggplot2","BTYD","BTYDplus","coda","optparse","arules","visNetwork","DT","shiny","shinydashboard","shinyjs","googleVis","ggplot2","scales","ggrepel","forcats","xts","plotly","logging","htmltools","qgraph","forecast"))'
3. Change the path mentioned in the previous section
4. Execute the riu_driver_slient.vbs

Description
requirements.txt:			The required python pacakges.
requirements_R.txt:			The required R pacakges.
107_daily_txn_driver.ps1:	The powershell script to extract daily transaction record from the SQL server. RUN ONLY IN THE SERVER.
107_daily_txn_selected.ps1: The powershell script to extract daily transaction record from the SQL server for the specified dates. RUN ONLY IN THE SERVER and IGNORE IT.
107_RFM_driver.ps1: 		The powershell script to run the RFM analysis. RUN ONLY IN THE SERVER AS A DAILY TASK.
107_LTV_driver.ps1: 		The powershell script to run the Customer Lifetime Value analysis. RUN ONLY IN THE SERVER AS A DAILY TASK.

./Clumpiness:				Not used at this stage.
./DataProcessor:			Python scripts to host the data processor as HTTP server.
./LTV						R scripts for the Lifetime value analysis.
./RFM						R scripts for the RFM analysis.
./Shiny						R scripts for the Demo.
./SQL						SQL scripts to extract data from SQL sever.
./Utils						R scripts for data manipulation.
./Visualizer				R scripts for plotting.
