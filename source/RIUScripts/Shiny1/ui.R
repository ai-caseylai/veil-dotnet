## ui.R ##
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(DT)
library(visNetwork)
library(plotly)


## Dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sbmenu",
    menuItem(HTML("Customer Segmentation<br/> 客户分级"), icon = icon("dashboard"),
             startExpanded = TRUE, 
             menuSubItem(HTML("Overview<br/> 概要"), tabName = "rfm-overview", selected = TRUE),
             menuSubItem(HTML("Characteristics<br/> 特性"), tabName = "rfm-character"),
             menuSubItem(HTML("Segment Transition<br/> 等级动态"), tabName = "rfm-transition"),
             menuSubItem(HTML("Customer Summary<br/> 顾客概要"), tabName = "rfm-customer-summary")),
    menuItem(HTML("Customer Lifetime Value<br/> 客户活跃度"), icon = icon("dashboard"),
             menuSubItem(HTML("Overview<br/> 概要"), tabName = "ltv-overview"),
             menuSubItem(HTML("Spending<br/> 支出"), tabName = "ltv-revenue"),
             menuSubItem(HTML("Visit<br/> 浏览"), tabName = "ltv-visit"),
             menuSubItem(HTML("Customer Summary<br/> 顾客概要"), tabName = "ltv-customer-summary")),
    menuItem(HTML("Customer Profile<br/> 客户资料"), icon = icon("dashboard"),
             tabName = "customer-profile"),
    menuItem(HTML("Price Sensitivity<br/> 价格敏感度"), icon = icon("area-chart"),
             tabName = "price-sensitivity"),
    menuItem(HTML("External Factors<br/> 外部因素"), icon = icon("dashboard"),
             menuSubItem(HTML("Demographic<br/> 人口"), tabName = "exo-demographic"),
             menuSubItem(HTML("Tourism<br/> 旅游"), tabName = "exo-tourism"),
             menuSubItem(HTML("Weather<br/> 天气"), tabName = "exo-weather")),
    # menuItem(HTML("Customer Activity<br/> 顾客活跃度"), icon = icon("dashboard"),
    #          tabName = "customer-activity"),
    menuItem("Setting", tabName = "setting", icon = icon("cog", 
                                                         lib = "glyphicon"))
  )
)


## RFM Overview layout
rfmOverview <- fluidPage(
    fluidRow(
      column(12,
             h3(HTML("Number of Customers by Segment <br/>不同等级客户的数量")),
             dataTableOutput("noOfCustomerPerSegmemtTable"),
             fixedRow(
              htmlOutput("noOfCustomerPerSegmemtPie", inline = FALSE)
             )
      )
    )
)


## RFM Characteristics layout
rfmCharacteristics <- fluidPage(
  fluidRow(
    column(12,
           h3(htmlOutput("noOfCustomerByStatsTitle", inline = TRUE),
              actionButton("noOfCustomerByStatsBtn", "", 
                           icon = icon("exchange"))),
           selectInput("rfmCharacteristicsSegmentSelector", "Segment:", 
                       c("All"), selected = "All"),
           fixedRow(
             htmlOutput("noOfCustomerByStats", inline = FALSE)
           ),
           h3(htmlOutput("avgStatsTitle", inline = TRUE),
              actionButton("avgStatsBtn", "", icon = icon("exchange"))
           ),
           checkboxInput("avgStatBySex", "Group by Gender", TRUE),
           fixedRow(
            htmlOutput("avgStatPlot", inline = FALSE)
           )
    )
  )
)


## RFM Transition layout
rfmTransition <- fluidPage(
  fluidRow(
    column(12,
           h3(HTML("Segment Transition Probabilities <br/>客户升降级的可能性")),
           fixedRow(
             # imageOutput("rfmTransProb", height = 640),
             column(10, visNetworkOutput("rfmTransProb")),
             column(2, uiOutput("rfmTransProbLegend"))
           ),
           h3(HTML("Customer Segmentation Prediction <br/>预测每月不同等级的客户数量")),
           fixedRow(
            imageOutput("rfmPrediction", height = 640)
           )
           #  , h3("Prediction Performance"),
           # fixedRow(
           #   imageOutput("rfmPredictionPerformance", height = 640)
           # )
    )
  )
)


## RFM Customer Summary layout
rfmCustomerSummary <- fluidPage(
  fluidRow(
    column(12,
           h3(HTML("Customer Summary <br/>客户详细数据")),
           downloadButton('downloadRFM', 'Download'), 
           dataTableOutput("rfmCustomerSummaryTable")
    )
  )
)

## LTV Overview layout
ltvOverview <- fluidPage(
  fluidRow(
    column(12,
           h3(HTML("Average Customer Lifetime Value by Segment <br/>不同等级客户的平均活跃度")),
           htmlOutput("avgLTVperSegment", inline = FALSE)
    )
  )
)


## LTV Revenue layout
ltvRevenue <- fluidPage(
  fluidRow(
    column(12,
           h3(HTML("Average Spending per Transaction by Segment </br>不同等级客户的平均支出")),
           imageOutput("avgRevenueperSegment", height = 640)
    )
  )
)


## LTV Visit layout
ltvVisit <- fluidPage(
  fluidRow(
    column(12,
           h3(HTML("Tracking Weekly Transactions <br/>交易量监控")),
           imageOutput("btydTrackingTransaction", height = 640)
           #  , h3("Prediction of Number of Visits by Segment"),
           # imageOutput("avgVisitperSegment", height = 640)
    )
  )
)


## LTV Customer Summary layout
ltvCustomerSummary <- fluidPage(
  fluidRow(
    column(12,
           h3(HTML("Customer Summary <br/>客户详细数据")),
           downloadButton('downloadLTV', 'Download'), 
           dataTableOutput("LTVCustomerSummaryTable")
    )
  )
)


## Customer Profile layout
customerProfile <- fluidPage(
  fluidRow(
    column(12,
           box(
             h3("Customer Profile"),
             p("The customer details, including RFM score, 
             lifetime value, are printed here.")
           )
    )
  ),
  fluidRow(
    column(6,
           box(
            h3("This customer may like:"),
            p("This part shows 5 recommended items.")
           )
    ),
    column(6,
           box(
            h3("Customer's journey:"),
            p("This part visualizes the selected customer's transaction record.")
           )
    )
  )
)


## LTV Visit layout
priceSensitivity <- fluidPage(
  fluidRow(
    column(12,
           h3("Purchase Behaviour under Discount"),
           imageOutput("priceSensitivity", height = 640)
    )
  )
)


## External Factor (Demographic) layout
exoDemographic <- fluidPage(
  fluidRow(
    column(12,
           h3("Demographic"),
           imageOutput("populationAge", height = 640),
           imageOutput("labourForce", height = 640)
    )
  )
)

## External Factor (Tourism) layout
exoTourism <- fluidPage(
  fluidRow(
    column(12,
           h3("Tourism"),
           imageOutput("hotelRooms", height = 640)
    )
  )
)

## External Factor (Weather) layout
exoWeather <- fluidPage(
  fluidRow(
    column(12,
           h3("Total Number of Transactions by Month"),
           imageOutput("totalTxnByMonth", height = 640)
    )
  )
)


## Customer Activity layout
cutomerActivity <- fluidPage(
  fluidRow(
    column(12,
           h3("Customer Activity"),
           htmlOutput('customerActivity')
           # plotlyOutput("customerActivity")
           # h3("Store Activity"),
           # plotlyOutput("storeActivity")
    )
  )
)


## Setting layout (hidden)
setting <- fluidPage(
  titlePanel("Setting"),
  fluidRow(
    column(3,
           h3("RFM setting"),
           selectInput("rfmPeriod", "Period", c("1 Year", "6 Months"))
    )
  )
)


## Dashboard body
body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem(tabName = "rfm-overview", rfmOverview),
    tabItem(tabName = "rfm-character", rfmCharacteristics),
    tabItem(tabName = "rfm-transition", rfmTransition),
    tabItem(tabName = "rfm-customer-summary", rfmCustomerSummary),
    tabItem(tabName = "ltv-overview", ltvOverview),
    tabItem(tabName = "ltv-revenue", ltvRevenue),
    tabItem(tabName = "ltv-visit", ltvVisit),
    tabItem(tabName = "ltv-customer-summary", ltvCustomerSummary),
    tabItem(tabName = "customer-profile", customerProfile),
    tabItem(tabName = "price-sensitivity", priceSensitivity),
    tabItem(tabName = "exo-demographic", exoDemographic),
    tabItem(tabName = "exo-tourism", exoTourism),
    tabItem(tabName = "exo-weather", exoWeather),
    # tabItem(tabName = "customer-activity", cutomerActivity),
    tabItem(tabName = "setting", setting)
  )
)


# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Demo"),
  sidebar,
  body
)
