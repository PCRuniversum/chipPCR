library(shiny)

shinyUI(pageWithSidebar(

  headerPanel("Amplification curve simulation"),

  sidebarPanel(
  numericInput("cycles", "Cycles", 35, min = 10, max = 60),
  numericInput("b.eff", "Efficiency", -25), 
  numericInput("bl", "Baseline", 0.05), 
  numericInput("ampl", "Amplitude", 1), 
  numericInput("Cq", "Cq value", 20),
  checkboxInput("noise", "Use noise in simulation", FALSE),
  numericInput("nnl", "Noise level", 0.025, min = 0, max = 10, step = 0.025),
  selectInput("nnl.method", "Variable:",
                list("Constant" = "constant", 
                     "Decreasing" = "decrease", 
                     "Increasing" = "increase")),
	hr(),
        helpText("The function AmpSim is a simple simulator for 
		  amplification reaction. Parameters: b.eff and Cq are 
		  most connected. Changing one of them will change 
		  both values. Cq can be used to define an approximate 
		  Cq value. The expression 'approximate Cq value' is 
		  used here because the actual Cq value is dependent on 
		  the users preferred method (e.g., Cy0 method, Second 
		  Derivative Maximum (SDM) method, threshold method). The 
		  funtion can be used to see how an experimental system 
		  compares to a predicted model. Moreover it can be used 
		  to simulate data with noise, missing values (NA), 
		  signal-to-noise ratios, photo-bleaching and other 
		  influences on a PCR reaction.")
  ),

    mainPanel(
      tabsetPanel(
        tabPanel("Amplification plots", plotOutput("AmpSimPlot"), 
                 verbatimTextOutput("bgSummary"), plotOutput("inderPlot")),
        tabPanel("Amplification data", tableOutput("bgTable"))
      )
    )
  )
)
