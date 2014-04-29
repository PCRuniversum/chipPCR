effcalc <- function(x, y, logx = TRUE, CV = FALSE, 
		    xlab = "log10(Concentration)", ylab = "Cq", 
		    main = "Efficiency Plot", RSD = FALSE, rob = FALSE, 
		    trend = TRUE, res.fit = TRUE, show.res = TRUE, type = "p", 
		    pch = 19, length = 0.05, col = "black"){
		    
  #Define if "robust" or standard function should be used as measures
  options(warn = -1)
  #Test if x and y exist.
  if (is.null(x)) 
   stop("Enter Concentration")
  if (is.null(y)) 
   stop("Enter Cq data")
  
  x.tmp <- log10(x)
  
  if (rob) {
            loc.fct <- median
	    dev.fct <- mad
	    } else {
		    loc.fct <- mean
		    dev.fct <- sd
		    }
		    
  y.m <- apply(y, 1, loc.fct)
  y.sd <- apply(y, 1, dev.fct)
	
  if (RSD) {
	    y.cv <- (y.sd / y.m) * 100
	   } else {
		   y.cv	<- y.sd / y.m
		  }
  
  # Aggregate calculated data and check for consistency
  # of the concentration (check if Na of Inf values
  # were produce and exclude these from further
  # calculation, check if at least two values for the
  # the linear regression are present.).
  
  res <- data.frame(x.tmp, y.m, y.sd, y.cv)
  res <- res[which(res[, 1]  >= 0), ]
  if (nrow(res) < 2) {
    stop("Can not perform calculation. At least two
	  dilutions required.")
  }
  if (nrow(res) == 2) {
    warning("At least three dilutions should be used to 
	    determine an amplificantion efficiency.")
  }
  
  # Decide which type of measure (e.g., mean vs. median, 
  # standard deviation vs. standard error) are shown in
  # the plot.
  if (rob == TRUE && RSD == FALSE) {
    names(res) <- c("Concentration", "Location (Median)", 
		    "Deviation (MAD)", 
		    "Coefficient of Variance (RSD [%])")
  }
  
  if (rob == FALSE && RSD == FALSE) {
    names(res)	<- c("Concentration", "Location (Mean)", 
		     "Deviation (SD)", 
		     "Coefficient of Variance (RSD [%])")
  }

  if (rob == TRUE && RSD == TRUE) {
    names(res)	<- c("Concentration", "Location (Median)", 
		      "Deviation (MAD)", 
		      "Coefficient of Variance (RSD)")
  }
  if (rob == FALSE && RSD == TRUE) {
    names(res)	<- c("Concentration", "Location (Mean)", 
		     "Deviation (SD)", 
		     "Coefficient of Variance (RSD)")
  }
  
  # Extract coordinates from data matrix
  coords <- c(min(res[, 1]), max(res[, 1]), 
	      min(res[, 2]), max(res[, 2])
	      )
  # Store default graphic parameters
  default.par <- par()
  
  # Perform a linear regression based on the values of the 
  # calculated mean/median
  lm.log <- lm(res[, 2] ~ res[, 1])
  Rsqured <- round(summary(lm.log)[["r.squared"]], 3)
  
  # Calculate the amplification efficiency (in percent)
  AE <- round(10^(-1/coef(lm.log)[2])/ 2 * 100, 1)
  
  # Calculate correlation between the concentration and 
  # the Cq values along with the significance level
  cortest <- cor.test(res[, 1], res[, 2])
  if (cortest$p.value < 0.001) {
      sign.out <- "; p < 0.001"
  }
  if (0.001 <= cortest$p.value && cortest$p.value < 0.01) {
      sign.out <- "; p < 0.01"
  }
  if (0.01 <= cortest$p.value && cortest$p.value < 0.05) {
      sign.out <- "; p < 0.05"
  }
  if (cortest$p.value >= 0.05) {
      sign.out <- "; p > 0.05"
  }
  
  # Plot the Coefficient of Variance
  plot(res[, 1], res[, 2], ylim = c(min(res[, 2] - res[, 3]), 
	max(res[, 2] + res[, 3])), xlab = xlab, 
	ylab = ylab, type = type, pch = pch, col = col,
	main = main
	)
  # Add error bar to the location parameters
  arrows(res[, 1], res[, 2] + res[, 3], res[, 1], 
	  res[, 2] - res[, 3], angle = 90, code = 3, 
	  length = length, col = col)
  
  # Add trend line of linear regression to plot
  if (trend) {
    abline(lm.log)
  }
  
  # Add "legend" with amplification efficiency, goodness of fit to plot
  if (res.fit) {
      text(coords[2] * 0.95, coords[4] * 0.95, 
	  paste("Efficiency = ", AE, " %", "\n",
	  "R^2 = ", Rsqured, "\n",
	  "r = ", round(cortest$estimate, 3), sign.out, "\n",
	  sep = ""), cex = 1.1
	  )
  }
  
  # Restore default graphic parameters
  par(default.par)

  # res is the an object of the type data.frame containing the 
  # concentration, location, deviation and coefficient of variance.
  if (show.res) {
      return(list(res = res, amplification.efficiency = AE, regression = lm.log, 
	      correlation.test = cortest))
  }
}
