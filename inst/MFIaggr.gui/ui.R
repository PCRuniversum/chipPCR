library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("MGIaggr"),
  sidebarPanel(
    fileInput("input.file", "Choose CSV File",
              accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    radioButtons("csv.type", "Type of csv file",
                 c("Dec: dot (.), Sep: comma (;)" = "csv1",
                   "Dec: comma (.), Sep: semicolon (;)" = "csv2")),
    numericInput("cyc.col", "Column containing the cycle data:", 1,
                 min = 1, step = 1),
    checkboxInput("RSD", "Relative standard deviation", TRUE),
    checkboxInput("rob", "Median and MAD", TRUE),
    numericInput("llul.low", "Region of interest - lower border:", 1,
                 min = 1, step = 1),
    numericInput("llul.up", "Region of interest - lower border:", 10,
                 min = 1, step = 1),
    
    downloadButton("download.result", "Download MFIaggr results (with graphics)"),
    downloadButton("download.table", "Download MFIaggr results (.csv)")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Input data", tableOutput("input.data")),
      tabPanel("Results with graphics", plotOutput("refMFI.plot"), 
               verbatimTextOutput("refMFI.summary")),
      tabPanel("Results - table", tableOutput("refMFI.table")),
      tabPanel("Results - data table", tableOutput("refMFI.data"))
    )
  )
)
)
