CPP <- function(x, y, smoother = TRUE, method = "savgol", trans = FALSE, 
		rob.reg = "lmrob", bg.outliers = FALSE, median = FALSE, 
		norm = "none", qnL = 0.03, amptest = FALSE, manual = FALSE, 
		nl = 0.08, ...) {
  
  testxy(x, y)
  # Test meaningfulness of qnL
  if (qnL <= 0.001 || qnL >= 0.999) 
    stop("qnL must be within 0.001 and 0.999.")
  
  # Select a method for the smoothing
  
  method.norm <- tolower(norm)
  if (grepl(method.norm, "none"))
    method.norm <- "none"
  if (grepl(method.norm, "luqn")) 
    method.norm <- "luqn"
  if (grepl(method.norm, "minmax"))
    method.norm <- "minmax"
  if (grepl(method.norm, "max"))
    method.norm <- "max"
  if (grepl(method.norm, "zscore"))
    method.norm <- "zscore"
  if (!(method.norm %in% c("none", "luqn", "minmax", "max", "zscore")))
    stop("Invalid method chosen.")
    
  # Define the method for the linear regression
  # lmrob (robustbase) uses MM-type estimators, rfit (Rfit)
  # uses a rank-based estimation model for linear regression,
  # and lm (stats) an ordinary least squares 
  
  method.reg <- tolower(rob.reg)
  if (grepl(method.reg, "lmrob"))
    method.reg <- "lmrob"
  if (grepl(method.reg, "rfit")) 
    method.reg <- "rfit"
  if (grepl(method.reg, "least")) 
    method.reg <- "least"
  if (grepl(method.reg, "rq")) 
    method.reg <- "rq"
  if (!(method.reg %in% c("lmrob", "rfit", "least", "rq")))
    stop("Invalid regression method chosen.")
  
  # Define the method for the smoothing
  # functionality identical to smoother function
  method.smooth <- tolower(method)
  if (grepl(method.smooth, "savgol"))
    method.smooth <- "savgol"
  if (grepl(method.smooth, "lowess")) 
    method.smooth <- "lowess"
  if (grepl(method.smooth, "mova")) 
    method.smooth <- "mova"
  if (grepl(method.smooth, "smooth")) 
    method.smooth <- "smooth"
  if (grepl(method.smooth, "spline")) 
    method.smooth <- "spline"
  if (grepl(method.smooth, "supsmu")) 
    method.smooth <- "supsmu"
  if (grepl(method.smooth, "whit1")) 
    method.smooth <- "whit1"
  if (grepl(method.smooth, "whit2")) 
    method.smooth <- "whit2"
  if (!(method.smooth %in% c("savgol", "lowess", "mova", "smooth", "spline", 
			     "supsmu", "whit1", "whit2")))
    stop("Invalid smoothing/filter method chosen.")
  
  lm.fit <- function(y, x, method) {
    switch(method,
           lmrob = do.call(function(x, y) lmrob(y ~ x), c(list(x = x, y = y))),
           rfit = do.call(function(x, y) rfit(y ~ x), c(list(x = x, y = y))),
           least = do.call(function(x, y) lm(y ~ x), c(list(x = x, y = y))),
           rq = do.call(function(x, y) rq(y ~ x), c(list(x = x, y = y)))
    )	
  }
  
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
  
  # Invoke smoother to improve data for further calculations
  # SERVE BUG (Priority high): "mova" will caus problems because it 
  # truncates the data (mova 3 -> first and last value miss at the end)
  
  if (smoother) {
    y <- smoother(x, y, trans = FALSE, bg.outliers = FALSE, 
			method = method.smooth, CPP = FALSE, ...)
    #some smoothing methods (for example mova) can introduce missing values
    if(any(is.na(y))) {
      y <- fixNA(x, y, spline = TRUE)
      message(paste0("NA values introduced by smoothing method ", 
                     method.smooth, ". fixNA() used."))
    }
  } else {
      y
  }
  
  # Test if linear correction based on the background range is requested
  # If requested first try a robust linear regression. If robust linear 
  # regression fails try standard lm()
  if (trans) {
    if (class(try(lm.fit(y[c(BG)], x[c(BG)], method = method.reg), 
				  silent = TRUE)) == "try-error") { 
      coefficients <- data.frame(lm.fit(y[c(BG)], x[c(BG)], 
					  method = "least")[1]) 
    } else { 
      lmrob.control <- suppressWarnings(lm.fit(y[c(BG)], x[c(BG)], 
					  method = method.reg)) 
      if ((class(lmrob.control) != "try-error") && 
            (lmrob.control$converged == TRUE)) { 
        coefficients <- data.frame(lm.fit(y[c(BG)], x[c(BG)], 
					  method = method.reg)[1]) 
      } else { 
        coefficients <- data.frame(lm.fit(y[c(BG)], x[c(BG)], 
					  method = "least")[1]) 
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
           minmax = do.call(function(y) (y - min(y)) / (max(y) - min(y)), 
			    c(list(y = y))),
	   max = do.call(function(y) (y / max(y)), 
			    c(list(y = y))),
           luqn = do.call(function(y, qnL) (y - quantile(y, qnL)) / 
			  (quantile(y, 1  - qnL) - quantile(y, qnL)), 
			  c(list(y = y, qnL = qnL))),
           zscore = do.call(function(y) (y - mean(y)) / sd(y), 
			    c(list(y = y)))
    )	
  }
  
  y.norm <- normalizer(y = y.norm, method = method.norm, qnL = qnL)
  
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


setGeneric("CPP")

  
setMethod("CPP", signature(x = "data.frame", y="missing"), 
          function(x, y, smoother = TRUE, trans = FALSE, rob.reg = "lmrob", 
		   bg.outliers = FALSE, median = FALSE, 
                   norm = "none", qnL = 0.03, 
                   amptest = FALSE, manual = FALSE, nl = 0.08, ...) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            CPP(x[, 1], x[, 2], trans = trans, bg.outliers = bg.outliers, 
                median = median, norm = norm, qnL = qnL,
                amptest = amptest, manual = manual, nl = nl, ...)
          })

setMethod("CPP", signature(x = "matrix", y="missing"), 
          function(x, y, smoother = TRUE, trans = FALSE, rob.reg = "lmrob", 
		   bg.outliers = FALSE, median = FALSE, 
                   norm = "none", qnL = 0.03, 
                   amptest = FALSE, manual = FALSE, nl = 0.08, ...) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            CPP(x[, 1], x[, 2], smoother = TRUE, trans = trans, 
		rob.reg = "lmrob", bg.outliers = bg.outliers, 
                median = median, norm = norm, qnL = qnL,
                amptest = amptest, manual = manual, nl = nl, ...)
          })
