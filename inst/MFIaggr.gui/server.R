library(shiny)
library(chipPCR)

#blank plot
bp <- function()
  plot(1, type="n", axes=F, xlab="", ylab="", main = "No input data")



# server for the Shiny app
shinyServer(function(input, output) {
  
  null.input <- reactive({
    is.null(input[["input.file"]])
    })
  
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
    if (null.input()) {
      data.frame(X1 = "No input data")
    } else {
      processed.data()
    }
  })
  
  output[["refMFI.plot"]] <- renderPlot({
    if (null.input()) {
      bp()
    } else {
      plot(res.mfi())
    }
  })
  
  output[["allp.plot"]] <- renderPlot({
    if (null.input()) {
      bp()
    } else {
      dat <- processed.data()
      plotCurves(dat[[input[["cyc.col"]]]], dat[, -input[["cyc.col"]]], CPP = TRUE, type = "l")
    }
  })
  
  
  output[["refMFI.summary"]] <- renderPrint({
    if (null.input()) {
      print("No input data.")
    } else {
      summary(res.mfi())
    }
  })
  
  output[["refMFI.table"]] <- renderTable({
    if (null.input()) {
      data.frame(X1 = "No input data")
    } else {
      slot(res.mfi(), ".Data")
    }
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



