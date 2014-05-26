amptester <-
  function(y, manual = FALSE, noiselevel = 0.08, background = NULL) {
    testxy(x = y, y, both = FALSE)
    # Test if background has only two values
    if (!is.null(background) && length(background) != 2)
        stop("Use only two values (e.g., background = c(1,10)) \n\t to set the range for the background correction")
    if (is.null(background) && manual == TRUE)
      stop("Manual test requires specified background.") 
    if (!is.null(background) && manual == FALSE)
	stop("Background is not empty. Manual test needs to be confirmed \n\t (manual = TRUE) to be performed.")
    #if background is NULL, sorting it is pointless and invokes warning
    if (!is.null(background))
      background <- as.integer(sort(background))
    
    # fix possible missing vaues with fixNA (spline method)
    y <- fixNA(1L:length(y), y)
    
    # FIRST TEST
    # Simple test if data come from noise or presumably a melting curve
    noisy <- FALSE
    res.shapiro <- shapiro.test(y)[["p.value"]]
    if (res.shapiro >= 5e-04) {
	message("The distribution of the curve data indicates noise.\n\t The data should be visually inspected.")
      noisy <- TRUE
      mess.shapiro <- "Appears not to be an amplification curve"
    } else {
      mess.shapiro <- "Appears to be an amplification curve"
    }
    
  # SECOND TEST
  # Linear Regression test (LRt)
  # This test determines the R^2 by a linear regression. The R^2 are
  # determined from a run of circa 15 percent range of the data.
  # If a sequence of more than six R^2s is larger than 0.8 is found 
  # thant is likely a nonlinear signal. This is a bit counterintuitive 
  # because R^2 of nonlinear data should be low.

  ws <- ceiling((15 * length(y)) / 100)
  if (ws < 5) 
	ws <- 5
  if (ws > 15) 
	ws <- 15
  y.tmp <- na.omit(y[-c(1:5)])
  x <- 1:length(y.tmp)
  suppressWarnings(
      res.reg <- sapply(1L:(length(y.tmp)), function (i)  {
      round(summary(lm(y.tmp[i:c(i + ws)] ~ x[i:c(i + ws)]))[["r.squared"]], 4)
      }
    )
  )
    
  # THIRD TEST
  #test if function is monotonic and growing during first few cycles
  nh <- trunc(length(y) * 0.2)
  cyc <- 1:nh
  resids <- residuals(lm(y[cyc] ~ cyc))
  decision <- ifelse(abs(cor(cyc, resids)) > 0.9, "positive", "negative")
  
  # FOURTH TEST
  # Binarize R^2 values. Everything larger than 0.8 is positve
  res.LRt <- res.reg
  # Define the limits for the R^2 test
  res.LRt[res.LRt < 0.8] <- 0
  res.LRt[res.LRt >= 0.8] <- 1
  # Seek for a sequence of at least six positve values (R^2 >= 0.8)
  # The first five measurepoitns of the amplification curve are skipped
  # beacuse most technologies and probetechnologies tend to overshot
  # in the start (background) region.
  res.out <- sapply(5L:(length(res.LRt) - 6), function(i) {
	ifelse(sum(res.LRt[i:(i + 4)]) == 5, TRUE, FALSE)
      }
    )
  
  # Test if more than one sequence of positive values was found (will 
  # be the case in most situation due to an overlap of the positive 
  # sequences.)
  linearity <- FALSE
  if (sum(res.out) >= 1) {
    linearity <- TRUE 
  }
  # FIFTH TEST (MANUAL)
  # Manual test for positve amplification based on a fixed threshold
  # value.
  if (manual) {
    signal <- median(y[-(background)]) - mad(y[-(background)])
    if (signal < noiselevel && (signal / noiselevel) < 1.25) {
      y <- abs(rnorm(length(y), 0, 0.1^30))
      decision <- "negative"
    } else {
	decision <- "positive"
      }
  } else {
    # FIFTH TEST (AUTOMATIC)
    # Apply a simple rule to take the first 20 percent and the last 15 percent
    # of any input data set to calculate the number of elements for the head 
    # (nh) and tail (nt), to deal with other data types like isothermal
    # amplifications

    #nh <- trunc(length(y) * 0.2)
    # nh calculated @ THIRD TEST
    nt <- trunc(length(y) * 0.15)

    if (t.test(head(y, n = nh), tail(y, n = nt), 
		alternative = "less")$p.value > 0.01) {
      y <- abs(rnorm(length(y), 0, 0.1^30))
      decision <- "negative"
    } else {
      decision <- "positive"
    }

    # SIXT TEST
    # The meaninfulness can be tested by comparison of the signals
    # 1) A robust "sigma" rule by median + 2 * mad 
    # 2) comparison of the signal/noise ratio. If less than 1.3 (30 percent) 
    # signal increase it is likely that nothing happened during the reaction.
    noisebackground <- median(head(y, n = nh)) + 2 * mad(head(y, n = nh))
    signal  <- median(tail(y, n = nt)) - 2 * mad(tail(y, n = nt))
    if (signal <= noisebackground || signal / noisebackground <= 1.25) {
      y <- abs(rnorm(length(y), 0, 0.1^30))
      decision <- "negative"
    } else {
      decision <- "positive"
    }
  }
  new("amptest", 
      .Data = y, 
      decision = decision, 
      noiselevel = noiselevel,
      noise = noisy,
      linearity = linearity,
      background = background)
}
