effcalc <- function(x, y, logx = TRUE, CV = FALSE, 
		    xlab = "log10(Concentration)", ylab = "Cq", 
		    main = "Efficiency Plot", RSD = FALSE, rob = FALSE, 
		    trend = TRUE, res.fit = TRUE, CI = FALSE, 
		    level = 0.95, show.res = TRUE, type = "p", 
		    pch = 19, length = 0.05, col = "black") {
		    
  testxy(x, y, txt.x = "Enter Concentration", txt.y = "Enter Cq data", 
	  length = FALSE)

  
  if (logx) {
    x.tmp <- log10(x)
  } else {
     x.tmp <- x
    }
  
  if (rob) {
    loc.fct <- median
    dev.fct <- mad
  } else {
      loc.fct <- mean
      dev.fct <- sd
    }
		    
  if (ncol(data.frame(y)) > 1) {
    y.m <- apply(y, 1, loc.fct)
    y.sd <- apply(y, 1, dev.fct)
  } else {
     y.m <- y
     y.sd <- rep(0, length(y))
  }
  	
  if (RSD) {
    y.cv <- (y.sd / y.m) * 100
  } else {
      y.cv <- y.sd / y.m
    }
  
  # Aggregate calculated data and check for consistency
  # of the concentration (check if Na of Inf values
  # were produce and exclude these from further
  # calculation, check if at least two values for the
  # the linear regression are present.).
  
  res <- data.frame(x.tmp, y.m, y.sd, y.cv)
  res <- res[which(is.finite(res[, 1])), ]
  
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
  lm.res <- lm(res[, 2] ~ res[, 1])
  # Calculate goodness of fit
  Rsqured <- round(summary(lm.res)[["r.squared"]], 3)
  
  # Calculate the amplification efficiency (in percent)
  AE <- round(10^(-1/coef(lm.res)[2])/ 2 * 100, 1)

  # Calculate correlation between the concentration and 
  # the Cq values along with the significance level
  cortest <- cor.test(res[, 1], res[, 2], conf.level = level)
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
  
  # Add "legend" with amplification efficiency, goodness of fit to plot
  # ToDo: expression(italic(R)^2 == Rsqured) for ... "R^2 = ", Rsqured ...?
  if (res.fit) {
      main <- paste0("Efficiency = ", AE, " %", "\n",
		    "R^2 = ", Rsqured, "\n",
		    "r = ", round(cortest$estimate, 3), sign.out, "\n"
		    )
  }
  # Plot the Coefficient of Variance
  plot(res[, 1], res[, 2], ylim = c(min(res[, 2] - res[, 3]), 
	max(res[, 2] + res[, 3])), xlab = xlab, 
	ylab = ylab, type = type, pch = pch, col = col,
	main = main
  )
  
  if (CI) {
  # add area and border lines of confidence interval
  # fix, does not yet work properly
  #polygon(c(rev(x.ci), x.ci),
  #	c(predict.ci[, 3], rev(predict.ci[, 2])),
  #	col = "lightgrey", border = NA)
      # prediction and parameters for confidence interval
  x.ci <- seq(coords[1], coords[2], length.out= nrow(res))
  predict.ci <- predict.lm(lm.res, newdata = data.frame(x.ci), 
			interval = "confidence", level = level)
			
    lines(rev(x.ci), predict.ci[ ,2], col = "lightblue")
    lines(rev(x.ci), predict.ci[ ,3], col = "lightblue")
  }
  # Add error bar to the location parameters
  arrows(res[, 1], res[, 2] + res[, 3], res[, 1], 
	 res[, 2] - res[, 3], angle = 90, code = 3, length = length, 
	 col = col)
  
  # Add trend line of linear regression to plot
  if (trend) {
    abline(lm.res)
  }
  
  # Restore default graphic parameters
  par(default.par)

  # res is the an object of the type data.frame containing the 
  # concentration, location, deviation and coefficient of variance.
  if (show.res) {
      return(list(res = res, amplification.efficiency = AE, 
		  regression = lm.res, correlation.test = cortest))
  }
}

setGeneric("effcalc")

setMethod("effcalc", signature(x = "data.frame", y="missing"), 
          function(x, y, logx = TRUE, CV = FALSE, 
                   xlab = "log10(Concentration)", ylab = "Cq", 
                   main = "Efficiency Plot", RSD = FALSE, rob = FALSE, 
                   trend = TRUE, res.fit = TRUE, CI = FALSE, level = 0.95,
                   show.res = TRUE, type = "p", pch = 19, length = 0.05, 
                   col = "black") { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            effcalc(x[, 1], x[, 2], logx = logx, CV = CV, 
                    xlab = xlab, ylab = ylab, 
                    main = main, RSD = RSD, rob = rob, 
                    trend = trend, res.fit = res.fit, CI = CI, level = level,
                    show.res = show.res, type = type, pch = pch, 
                    length = length, col = col)
          })

setMethod("effcalc", signature(x = "matrix", y="missing"), 
          function(x, y, logx = TRUE, CV = FALSE, 
                   xlab = "log10(Concentration)", ylab = "Cq", 
                   main = "Efficiency Plot", RSD = FALSE, rob = FALSE, 
                   trend = TRUE, res.fit = TRUE, CI = FALSE, level = 0.95,
                   show.res = TRUE, type = "p", pch = 19, length = 0.05, 
                   col = "black") { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            effcalc(x[, 1], x[, 2], logx = logx, CV = CV, 
                    xlab = xlab, ylab = ylab, 
                    main = main, RSD = RSD, rob = rob, 
                    trend = trend, res.fit = res.fit, CI = CI, 
                    level = level, show.res = show.res, type = type, 
                    pch = pch, length = length, col = col)
          })