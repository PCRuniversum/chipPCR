library(shiny)

# server for the Shiny app
shinyServer(function(input, output) {
  # Create a plot
        # Create a plot
      output$AmpSimPlot <- renderPlot(
      {
        res.AmpSim <- chipPCR:::AmpSim(cyc = 1:input$cycles, 
				       b.eff = input$b.eff, 
				       bl = input$bl, ampl = input$ampl, 
				       Cq = input$Cq, noise = input$noise, 
				       nnl = input$nnl, 
				       nnl.method = input$nnl.method
				)
	
	# Use inder to calculate the SDM and alike
	res.inder <- chipPCR:::inder(res.AmpSim[, 1], res.AmpSim[, 2])
	res.sum <- summary(res.inder)
	
	# Render the curves
	par(mfrow = c(2,1))
	plot(res.AmpSim, main = "Simulated curve")
	
	plot(res.inder, xlab = "Cycles", ylab = "Fluorescence", 
	     main = "Calculation of curve parameters")
	}
    )
  }
)
