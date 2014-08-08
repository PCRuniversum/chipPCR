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
  
  
  res.amptest <- reactive({
    dat <- processed.data()
    res <- lapply(1L:ncol(dat), function(i)
      amptester(y = dat[, i], 
                manual = input[["amptester.manual"]]  == "manual",
                noiselevel = input[["amptester.noiselevel"]],
                background = c(input[["amptester.bcg1"]], input[["amptester.bcg2"]])))
    res
  })
  
  output[["input.data"]] <- renderTable({
    processed.data()
  })
  
  output[["amptester.summs.plots"]] <- renderUI({
    anal_list <- lapply(1L:length(res.amptest()), function(i) {
      list(plotOutput(paste0("plot", i)), verbatimTextOutput(paste0("summ", i)))
    })
    do.call(tagList, unlist(anal_list, recursive = FALSE))
  })
  
  
  for (i in 1L:300) {
    local({
      my_i <- i
      
      output[[paste0("plot", my_i)]] <- renderPlot(plot(res.amptest()[[my_i]]))
      output[[paste0("summ", my_i)]] <- renderPrint({
        cat(colnames(processed.data())[my_i], "\n")  
        summary(res.amptest()[[my_i]])                                          
      })
    })
  }
  
  
  output[["amptest.summary"]] <- renderUI({
    uiOutput("amptester.summs.plots")
  })
  
  output[["amptest.table"]] <- renderTable({
    dat <- data.frame(t(sapply(res.amptest(), function(i) summary(i))))
    for (i in c("tht.dec", "slt.dec", "rgt.dec", "shap.noisy", "lrt.test"))
      dat[, i] <- as.logical(dat[, i])
    dat
  })
  
  output[["amptest.data"]] <- renderTable({
    dat <- data.frame(sapply(res.amptest(), function(i) i))
    colnames(dat) <- colnames(processed.data())
    dat
  })
  
  output[["download.table"]] <- downloadHandler(
    filename = "amptester_report.csv",
    content <- function(file) {
      dat <- data.frame(t(sapply(res.amptest(), function(i) summary(i))))
      for (i in c("tht.dec", "slt.dec", "rgt.dec", "shap.noisy", "lrt.test"))
        dat[, i] <- as.logical(dat[, i])
      #improve it
      write.csv(dat, file)
    }
  )
  
  output[["download.data"]] <- downloadHandler(
    filename = "amptester_data.csv",
    content <- function(file) {
      dat <- data.frame(sapply(res.amptest(), function(i) i))
      colnames(dat) <- colnames(processed.data())
      #improve it
      write.csv(dat, file)
    }
  )
  
  output[["download.result"]] <- downloadHandler(
    filename  = "amptester_report.html",
    content <- function(file) {
      knitr:::knit(input = "amptester_report.Rmd", 
                   output = "amptester_report.md", quiet = TRUE)
      markdown:::markdownToHTML("amptester_report.md", file)
    }
  )
})



