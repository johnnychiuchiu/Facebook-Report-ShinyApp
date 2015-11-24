library(shiny)
library(rCharts)
library(dplyr)
library(stringr)
library(XLConnect)
library(data.table)

shinyUI(fluidPage(
  titlePanel("Facebook Report"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","上傳檔案"),
      tags$hr(),
      selectInput("y_input", label = h5("選擇廣告組合"),""),
      dateRangeInput('date_range','選擇日期'),
      tags$hr(),
      selectInput("creative_input", label = h5("選擇某一套素材"),"")
    ),
    mainPanel(
      uiOutput("tb")
    )
    
  )
))