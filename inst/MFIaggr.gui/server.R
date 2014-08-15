library(shiny)
library(chipPCR)

# server for the Shiny app
shinyServer(function(input, output) {
  
  processed.data <- reactive({
    dat <- switch(input[["csv.type"]], 
                  csv1 = read.csv(input[["input.file"]][["datapath"]], 
                                  header = input[["header"]]),
                  csv2 = read.csv2(input[["input.file"]][["datapath"]], 
                                   header = input[["header"]]))
    if(!input[["header"]])
      colnames(dat) <- paste0("Column", 1L:ncol(dat))
    dat
  })
  
  
  res.mfi <- reactive({
    dat <- processed.data()
    res <- MFIaggr(x = dat, cyc = input[["cyc.col"]],
                   fluo = (1L:ncol(dat))[-input[["cyc.col"]]], RSD = input[["RSD"]], 
                   rob = input[["rob"]], 
                   llul = c(input[["llul.low"]],input[["llul.up"]]))
    res
  })
  
  output[["input.data"]] <- renderTable({
    processed.data()
  })
  
  output[["refMFI.plot"]] <- renderPlot({
    plot(res.mfi())
  })
  
  output[["allp.plot"]] <- renderPlot({
    dat <- processed.data()
    m <- ncol(dat) - 1
    best.rows <- which.min(abs(m - (1L:(m/2))^2))
    plotCurves(dat[[input[["cyc.col"]]]], dat[, -input[["cyc.col"]]], 
               nrow = best.rows, CPP = TRUE, type = "l")
  })
  
  
  output[["refMFI.summary"]] <- renderPrint({
    summary(res.mfi())
  })
  
  output[["refMFI.table"]] <- renderTable({
    slot(res.mfi(), ".Data")
  })
  
  
  output[["download.table"]] <- downloadHandler(
    filename = "refMFI_report.csv",
    content <- function(file) {
      write.csv(slot(res.mfi(), ".Data"), file)
    }
  )
   
  output[["download.result"]] <- downloadHandler(
    filename  = "refMFI_report.html",
    content <- function(file) {
      knitr:::knit(input = "refMFI_report.Rmd", 
                   output = "refMFI_report.md", quiet = TRUE)
      markdown:::markdownToHTML("refMFI_report.md", file)
    }
  )
})



