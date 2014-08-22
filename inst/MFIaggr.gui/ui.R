library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("MFIaggr"),
  sidebarPanel(
    fileInput("input.file", "Choose CSV File (input should contain cycle data)",
              accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    radioButtons("csv.type", "Type of csv file",
                 c("Dec: dot (.), Sep: comma (;)" = "csv1",
                   "Dec: comma (.), Sep: semicolon (;)" = "csv2")),
    numericInput("cyc.col", "Column containing the cycle data:", 1,
                 min = 1, step = 1),
    checkboxInput("RSD", "Relative standard deviation:", FALSE),
    checkboxInput("rob", "Median and MAD:", FALSE),
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
      tabPanel("All curves plot", plotOutput("allp.plot"))
    )
  )
)
)
