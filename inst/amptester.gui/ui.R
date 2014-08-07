library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("amptester"),
  sidebarPanel(
    fileInput("input.file", "Choose CSV File",
              accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    radioButtons("csv.type", "Type of csv file",
                 c("Dec: dot (.), Sep: comma (;)" = "csv1",
                   "Dec: comma (.), Sep: semicolon (;)" = "csv2")),
    selectInput("amptester.manual", "Choose a test:", 
                choices = c("statistical", "manual"),
                selected = "statistical"),
    numericInput("amptester.noiselevel", "Noise level:", 0.08,
                 min = 0, max = 2, step = 0.04),
    numericInput("amptester.bcg1", "Background start:", 1,
                 min = 1, max = 20, step = 1),
    numericInput("amptester.bcg2", "Background end:", 6,
                 min = 1, max = 20, step = 1),
    downloadButton("download.result", "Download amptester results (with graphics)"),
    downloadButton("download.table", "Download amptester results (table)")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Input data", tableOutput("input.data")),
      tabPanel("Results with graphics", uiOutput("amptest.summary")),
      tabPanel("Results - table", tableOutput("amptest.table"))
    )
  )
)
)
