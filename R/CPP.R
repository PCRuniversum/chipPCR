CPP <- function(x, y, trans = TRUE, bg.outliers = FALSE, median = FALSE, 
                minmax = FALSE, qnL = 0.1, 
                amptest = FALSE, manual = FALSE, nl = 0.08) {

  # Test if x and y exist and have identical lengths.
  if (is.null(x)) 
    stop("Enter abscissa value")
  if (is.null(y)) 
    stop("Enter ordinate value")
  if (length(x) != length(y)) 
    stop("Use abscissa and ordinate data with same number of 
		elements")
  # Test meaningfulness of qnL
  if (qnL <= 0.001 || qnL >= 0.999) 
    stop("qnL must be within 0.001 and 0.999.")
  # Remove missing values from y
  y <- fixNA(x, y, spline = TRUE)
  
  #Try to detect background range automatically or manually
  bg <- bg.max(x, y)
  BG <- slot(bg, "bg.start"):slot(bg, "bg.stop")
  
  # Remove outliers (high and low) from the background range and 
  # substitute with the median
  if (bg.outliers) { 
    y[c(BG)] 	<- rm.outlier(y[c(BG)], fill = TRUE, 
                            median = median)
    y[c(BG)] 	<- rm.outlier(y[c(BG)], opposite = TRUE, 
                            fill = TRUE, median = median)
  } 
  # Test if linear correction based on the background range is requested
  # If requested first try a robust linear regression. If robust linear 
  # regression fails try standard lm()
  if (trans) {
    if (class(try(lmrob(y[c(BG)] ~ x[c(BG)]), 
                  silent = TRUE)) == "try-error") { 
      coefficients <- data.frame(lm(y[c(BG)] ~ x[c(BG)])[1]) 
    } else { 
      lmrob.control <- suppressWarnings(lmrob(y[c(BG)] ~ x[c(BG)])) 
      if ((class(lmrob.control)!="try-error") && 
            (lmrob.control$converged == TRUE)) { 
        coefficients <- data.frame(lmrob(y[c(BG)] ~ x[c(BG)])[1]) 
      } else { 
        coefficients <- data.frame(lm(y[c(BG)] ~ x[c(BG)])[1]) 
      } 
    }
    # Apply linear model to the raw data
    y.norm <- y - (coefficients[2, 1] * x + coefficients[1, 1])
    # Subtract the median (based on background range) from the data
    y.norm <- y.norm - median(y.norm[c(BG)]) 
    # Subtract the median (based on background range) from the data 
    # without a linear model
  } else {y.norm <- y - median(y[c(BG)])}
  
  # Perform a normalization to a specified quantile value
  if (minmax) {
    y.norm <- (y.norm - quantile(y.norm, qnL)) / 
      (quantile(y.norm, 1  - qnL) - quantile(y.norm, qnL)
      )
  }
  # Test if the amplifification is likely to be positive
  if (amptest) {
    y.norm <- amptester(y.norm, manual = manual, 
                        background = range(BG), 
                        noiselevel = nl)
  }

  list(y.norm = y.norm, BG = BG)
}
