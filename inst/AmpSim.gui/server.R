library(shiny)
library(chipPCR)

# server for the Shiny app
shinyServer(function(input, output) {
  res.AmpSim <- reactive({AmpSim(cyc = 1:input$cycles, 
                                 b.eff = input$b.eff, 
                                 bl = input$bl,
                                 ampl = input$ampl, 
                                 Cq = input$Cq,
                                 noise = input$noise, 
                                 nnl = input$nnl, 
                                 nnl.method = input$nnl.method)
  })
  # Use bg.max to calculate the SDM and alike
  res.bg <- reactive({bg.max(res.AmpSim())
  })
  # Create a plot
  
  output$AmpSimPlot <- renderPlot({
    plot(res.AmpSim(), main = "Simulated curve", type = "b")
  })
  
  output$inderPlot <- renderPlot({
    plot(res.bg(), main = "Calculation of curve parameters")
  })
  
  output$bgTable <- renderTable({
    res.bg()
  })
  
  output$bgSummary <- renderPrint({
    summary(res.bg())
  })
})

