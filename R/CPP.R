CPP <- function(x, y, trans = TRUE, bg.outliers = FALSE, median = FALSE, 
                minmax.m = "none", qnL = 0.1, 
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
  
  # Select a method for the normalization
  # "none" does basically nothing, "minmax" does a minimum maximum
  # normalization, "luqn" does a qauntile normalization based
  # on the qnL value and zscore is (y - mean(y))/sd(y)
  # Alternativ for minmax:
  # minmax <- function(x, lv = 0, tv = 1) {
  # ((x - min(x)) * (tv - lv) / (max(x) - min(x))) + lv 
  # }
  # scales between user defined ranges
  
  method <- tolower(minmax.m)
  if (grepl(method, "none"))
    method <- "none"
  if (grepl(method, "luqn")) 
    method <- "luqn"
  if (grepl(method, "minmax"))
    method <- "minmax"
  if (grepl(method, "zscore"))
    method <- "zscore"
  if (!(method %in% c("none", "luqn", "minmax", "zscore")))
    stop("Invalid method chosen.")
  
  # Remove missing values from y
  if(any(is.na(y)))
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
  normalizer <- function(y, method, qnL = qnL) {
    switch(method,
           none = do.call(function(y) y, c(list(y = y))),
           minmax = do.call(function(y) (y - min(y)) / (max(y) - min(y)), c(list(y = y))),
           luqn = do.call(function(y, qnL) (y - quantile(y, qnL)) / (quantile(y, 1  - qnL) - quantile(y, qnL)), c(list(y = y, qnL = qnL))),
           zscore = do.call(function(y) (y - mean(y)) / sd(y), c(list(y = y)))
    )	
  }
  
  y.norm <- normalizer(y = y.norm, method = method, qnL = qnL)
  
  # Test do give some output of the signal difference (backround vs. plateau)
  dB.y <- abs(quantile(y, 1 - qnL) / quantile(y, qnL))
  dB.res <- 10 * log(dB.y, base = 20)
  
  # Test if the amplifification is likely to be positive
  if (amptest) {
    y.norm <- amptester(y.norm, manual = manual, 
                        background = range(BG), 
                        noiselevel = nl)
  }
  
  list(y.norm = y.norm, BG = BG, dB = dB.res)
}
