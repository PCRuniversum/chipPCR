MFIaggr <- function(x, y, CV = FALSE, RSD = FALSE, rob = FALSE, 
		    errplot = TRUE, type = "p", pch = 19, length = 0.05, 
		    col = "black", llul = c(1,10)){
		    
  #Define if "robust" or standard function should be used as measures
  options(warn = -1)
  #Test if x and y exist.
  if (is.null(x)) 
   stop("Enter Cycle")
  if (is.null(y)) 
   stop("Enter fluorescence data")

  # Test if llul has only two values
  if (!is.null(llul) && length(llul) != 2)
   stop("Use two cycle values (e.g., llul = c(1,10)) to set 
        the range for the analysis.")
  
  llul <- sort(llul)	
  
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
  res <- data.frame(x, y.m, y.sd, y.cv)

  if (rob == TRUE && RSD == FALSE) {
    names(res) <- c("Cycle", "Location (Median)", 
		    "Deviation (MAD)", 
		    "Coefficient of Variance (RSD [%])")
  }
  
  if (rob == FALSE && RSD == FALSE) {
    names(res)	<- c("Cycle", "Location (Mean)", 
		     "Deviation (SD)", 
		     "Coefficient of Variance (RSD [%])")
  }

  if (rob == TRUE && RSD == TRUE) {
    names(res)	<- c("Cycle", "Location (Median)", 
		      "Deviation (MAD)", 
		      "Coefficient of Variance (RSD)")
  }
  if (rob == FALSE && RSD == TRUE) {
    names(res)	<- c("Cycle", "Location (Mean)", 
		     "Deviation (SD)", 
		     "Coefficient of Variance (RSD)")
  }
	
  coords <- c(min(x), max(x), min(y), max(y))
	
  stats <- c(round(mean(unlist(y[llul, ]), na.rm = TRUE), 3),
	    round(median(unlist(y[llul, ]), na.rm = TRUE), 3), 
	    round(sd(unlist(y[llul, ]), na.rm = TRUE), 2),
	    round(mad(unlist(y[llul, ]), na.rm = TRUE), 2)
	    )

	
  default.par <- par()
  
  #Plot the Coefficient of Variance

     if (errplot) {
	    if (CV) {
		par(fig = c(0,0.6,0,1))
		plot(res[, 1], res[, 4], xlab = "Cycle", ylab = "CV", 
		  type = type, pch = pch, col = col,
		  main = paste("ROI samples: ", ncol(y), "\n",
			       "ROI mean: ", stats[1], " +- ", stats[3], "\n",
			       "ROI median: ", stats[2], " +- ", stats[4],
			        sep = "")
		)
		
		abline(v = llul, col = "lightgrey")
		      #Plot the location with error bars.
		
		par(fig = c(0.65,1,0.5,1), new = TRUE)
		res.dens <- density(unlist(y[llul, ]))
		plot(res.dens, xlab = "RFU", main = paste("Cycle ", 
		      llul[1], " to ", llul[2], "\n", "bw ", 
		      round(res.dens$bw, 3), "\n", "N ", res.dens$n, 
		      sep = ""))
		
		par(fig = c(0.65,1,0,0.5), new = TRUE)
		qqnorm(unlist(y[llul, ]))
		qqline(unlist(y[llul, ]))

	    } else {
		par(fig = c(0,0.6,0,1))
		plot(res[, 1], res[, 2], ylim = c(min(res[, 2] - res[, 3]), 
		     max(res[, 2] + res[, 3])), xlab = "Cycle", 
		     ylab = "MFI", type = type, pch = pch, col = col,
		     main = paste("ROI samples: ", ncol(y), "\n",
			    "ROI mean: ", stats[1], " +- ", stats[3], "\n",
			    "ROI median: ", stats[2], " +- ", stats[4],
			  sep = ""))
		abline(v = llul, col = "lightgrey")
		
		arrows(res[, 1], res[, 2] + res[, 3], res[, 1], 
		       res[, 2] - res[, 3], angle = 90, code = 3, 
		       length = length, col = col)
		
		par(fig = c(0.65,1,0.5,1), new = TRUE)
		res.dens <- density(unlist(y[llul, ]))
		plot(res.dens, xlab = "RFU", main = paste("ROI cycle ", 
		      llul[1], " to ", llul[2], "\n", "bw ", 
		    round(res.dens$bw, 3), "\n", "N ", res.dens$n, 
		    sep = ""))
		par(fig = c(0.65,1,0,0.5), new = TRUE)
		res.qq <- qqnorm(unlist(y[llul, ]))
		qqline(unlist(y[llul, ]))
	    }
	  }
	
	  par(default.par)

	#res is the an object of the type data.frame containing the 
	#temperature, location, deviation and coefficient of variance.
	if (errplot == TRUE) {
	      return(list(res = res, density.res = res.dens, 
			  qqnorm.res = res.qq))
	    } else {
		    return(res = res)
		    }
}
