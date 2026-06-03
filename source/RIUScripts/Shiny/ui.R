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
    menuItem("Customer Segmentation", icon = icon("dashboard"), 
             startExpanded = TRUE, 
             menuSubItem("Overview", tabName = "rfm-overview", selected = TRUE),
             menuSubItem("Characteristics", tabName = "rfm-character"),
             menuSubItem("Segment Transition", tabName = "rfm-transition"),
             menuSubItem("Customer Summary", tabName = "rfm-customer-summary")),
    menuItem("Customer Lifetime Value", icon = icon("dashboard"),
             menuSubItem("Overview", tabName = "ltv-overview"),
             menuSubItem("Revenue", tabName = "ltv-revenue"),
             menuSubItem("Visit", tabName = "ltv-visit"),
             menuSubItem("Customer Summary", tabName = "ltv-customer-summary")),
    menuItem("Customer Profile", icon = icon("dashboard"), 
             tabName = "customer-profile"),
    menuItem("Price Sensitivity", icon = icon("area-chart"), 
             tabName = "price-sensitivity"),
    menuItem("External Factors", icon = icon("dashboard"),
             menuSubItem("Demographic", tabName = "exo-demographic"),
             menuSubItem("Tourism", tabName = "exo-tourism"),
             menuSubItem("Weather", tabName = "exo-weather")),
    menuItem("Customer Activity", icon = icon("dashboard"), 
             tabName = "customer-activity"),
    menuItem("What-if Analysis", icon = icon("sliders"), tabName = "whatif"),
    menuItem("Setting", tabName = "setting", icon = icon("cog", 
                                                         lib = "glyphicon"))
  )
)


## RFM Overview layout
rfmOverview <- fluidPage(
    fluidRow(
      column(12,
             h3("Number of Customers by Segment"),
             verbatimTextOutput("debugStatus"),
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
           h3(textOutput("noOfCustomerByStatsTitle", inline = TRUE), 
              actionButton("noOfCustomerByStatsBtn", "", 
                           icon = icon("exchange"))),
           uiOutput("noOfCustomerByStatsAllSegments"),
           h3(textOutput("avgStatsTitle", inline = TRUE),
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
           h3("Segment Transition Probabilities"),
           fixedRow(
             # imageOutput("rfmTransProb", height = 640),
             column(10, visNetworkOutput("rfmTransProb")),
             column(2, uiOutput("rfmTransProbLegend"))
           ),
           h3("Customer Segmentation Prediction"),
           fixedRow(
            imageOutput("rfmPrediction", height = 640)
           ),
           h3("Prediction Performance"),
           fixedRow(
             imageOutput("rfmPredictionPerformance", height = 640)
           )
    )
  )
)


## RFM Customer Summary layout
rfmCustomerSummary <- fluidPage(
  fluidRow(
    column(12,
           h3("Customer Summary"),
           downloadButton('downloadRFM', 'Download'), 
           dataTableOutput("rfmCustomerSummaryTable")
    )
  )
)

## LTV Overview layout
ltvOverview <- fluidPage(
  fluidRow(
    column(12,
           h3("Average Customer Lifetime Value by Segment"),
           htmlOutput("avgLTVperSegment", inline = FALSE)
    )
  )
)


## LTV Revenue layout
ltvRevenue <- fluidPage(
  fluidRow(
    column(12,
           h3("Average Revenue per Transaction by Segment"),
           imageOutput("avgRevenueperSegment", height = 640)
    )
  )
)


## LTV Visit layout
ltvVisit <- fluidPage(
  fluidRow(
    column(12,
           h3("Tracking Weekly Transactions"),
           imageOutput("btydTrackingTransaction", height = 640),
           h3("Prediction of Number of Visits by Segment"),
           imageOutput("avgVisitperSegment", height = 640)
    )
  )
)


## LTV Customer Summary layout
ltvCustomerSummary <- fluidPage(
  fluidRow(
    column(12,
           h3("Customer Summary"),
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


## What-if Analysis layout
whatif <- fluidPage(
  fluidRow(
    column(4,
      box(width = 12, title = "RFM Parameters", status = "primary", solidHeader = TRUE,
        sliderInput("wi_maxScore", "Max Score", min = 3, max = 10, value = 5, step = 1),
        sliderInput("wi_recencyWt", "Recency Weight", min = 1, max = 10, value = 4, step = 1),
        sliderInput("wi_frequencyWt", "Frequency Weight", min = 1, max = 10, value = 4, step = 1),
        sliderInput("wi_monetaryWt", "Monetary Weight", min = 1, max = 10, value = 4, step = 1),
        checkboxInput("wi_thres", "Apply minTxn threshold", value = TRUE),
        conditionalPanel("input.wi_thres == true",
          sliderInput("wi_minTxn", "Min Transactions", min = 1, max = 10, value = 2, step = 1)
        ),
        br(),
        actionButton("wi_apply", "Apply RFM Changes", icon = icon("refresh"), 
                     style = "color: #fff; background-color: #337ab7; width: 100%")
      ),
      box(width = 12, title = "Regenerate Data", status = "warning", solidHeader = TRUE,
        p("Re-run the full pipeline with new customer parameters."),
        actionButton("wi_regenerate", "Regenerate 1000 Customers", icon = icon("database"),
                     style = "color: #fff; background-color: #d9534f; width: 100%"),
        br(), br(),
        verbatimTextOutput("wi_regenerateStatus")
      )
    ),
    column(8,
      tabBox(width = 12,
        tabPanel("Segment Distribution",
          h4(textOutput("wi_segmentCount")),
          dataTableOutput("wi_segmentTable"),
          br(),
          htmlOutput("wi_segmentPie", inline = FALSE)
        ),
        tabPanel("Average Stats",
          h4(textOutput("wi_avgStatsTitle", inline = TRUE),
             actionButton("wi_avgStatsBtn", "", icon = icon("exchange"))),
          checkboxInput("wi_avgBySex", "Group by Gender", TRUE),
          htmlOutput("wi_avgStatPlot", inline = FALSE)
        ),
        tabPanel("Customer Distribution",
          h4(textOutput("wi_custDistTitle", inline = TRUE),
             actionButton("wi_custDistBtn", "", icon = icon("exchange"))),
          uiOutput("wi_custDistCharts")
        )
      )
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
    tabItem(tabName = "customer-activity", cutomerActivity),
    tabItem(tabName = "whatif", whatif),
    tabItem(tabName = "setting", setting)
  )
)


# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Demo"),
  sidebar,
  body
)
