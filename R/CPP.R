CPP <- function(x, y, trans = TRUE, bg.outliers = FALSE, median = FALSE, 
	 minmax = FALSE, qnL = 0.1, 
	 amptest = FALSE, manual = FALSE, nl = NULL) {
	tmp.warn <- getOption("warn")
  	options(warn = -1)
  	# Test if x and y exist and have identical lengths.
	if (is.null(x)) 
	   stop("Enter abscissa value")
	if (is.null(y)) 
	   stop("Enter ordinate value")
	if (length(x) != length(y)) 
	   stop("Use abscissa and ordinate data with same number of 
		 elements")
	if (qnL <= 0.001 || qnL >= 0.999) 
	    stop("qnL must be within 0.001 and 0.999.")
	    
  bg <- bg.max(x, y)
  BG <- bg[["bg.start"]]:bg[["bg.stop"]]
  y <- fixNA(x, y, spline = TRUE)
  if (trans) {
    if (class(try(lmrob(y[c(BG)] ~ x[c(BG)]), 
	silent = TRUE)) == "try-error") { 
      coefficients <- data.frame(lm(y[c(BG)] ~ x[c(BG)])[1]) 
    } else { 
      lmrob.control <- suppressWarnings(lmrob(y[c(BG)] ~ x[c(BG)])) 
      if ((class(lmrob.control)!="try-error") && (lmrob.control$converged == TRUE)) { 
	coefficients <- data.frame(lmrob(y[c(BG)] ~ x[c(BG)])[1]) 
      } else { 
	  coefficients <- data.frame(lm(y[c(BG)] ~ x[c(BG)])[1]) 
	} 
      } 
      y.norm <- y - (coefficients[2, 1] * x + coefficients[1, 1]) 
      y.norm <- y.norm - median(y.norm[c(BG)]) 
    } else {y.norm <- y - median(y[c(BG)])}
		    
    if (bg.outliers) { 
      y.norm[c(BG)] 	<- rm.outlier(y.norm[c(BG)], fill = TRUE, 
				      median = median)
      y.norm[c(BG)] 	<- rm.outlier(y.norm[c(BG)], opposite = TRUE, 
				      fill = TRUE, median = median)
    } 

    if (minmax) {
      y.norm <- (y.norm - quantile(y.norm, qnL)) / 
		(quantile(y.norm, 1  - qnL) - quantile(y.norm, qnL))
    }
    if (amptest) {
      amptester(y.norm, manual = manual, background = BG, noiselevel = nl)
    }
    options(warn = tmp.warn)
    return(list(y.norm = y.norm, BG = BG))
}
