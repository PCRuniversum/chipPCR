library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("amptester"),
  sidebarPanel(
    fileInput("input.file", "Choose CSV File",
              accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    radioButtons("csv.type", "Type of csv file",
                 c("Dec: dot (.), Sep: comma (;)" = "csv1",
                   "Dec: comma (.), Sep: semicolon (;)" = "csv2"))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Input data", tableOutput("input.data")),
      tabPanel("Analysis", uiOutput("amptest.summary"))
    )
  )
)
)
