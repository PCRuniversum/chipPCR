amptester <-
  function(y, manual = FALSE, noiselevel = 0.08, background = NULL) {
    testxy(x = y, y, both = FALSE)
    # Test if background has only two values
    if (!is.null(background) && length(background) != 2)
      stop("Use only two values (e.g., background = c(1,10)) 
		to set the range for the background correction")
    if (is.null(background) && manual)
      stop("Manual test requires specified background.") 
    
    #if background is NULL, sorting it is pointless and invokes warning
    if (!is.null(background))
      background <- as.integer(sort(background))
    
    # fix possible missing vaues with fixNA (spline method)
    y <- fixNA(1L:length(y), y)
    
    # Simple test if data come from noise or presumably a melting curve
    res.shapiro <- shapiro.test(y)$p.value
    if (res.shapiro >= 0.001) {
	    message("The distribution of the curve data indicates noise.
		     \nThe data should be visually inspected.")
	    mess.shapiro <- "Appears not to be an amplification curve"
    } else {
	    mess.shapiro <- "Appears to be an amplification curve"
    }
    
    if (manual) {
      noisebackground <- mean(y[background]) + 5 * sd(y[background])
      signal  <- mean(y[-(background)])
      if (signal <= noiselevel) {
        y <- abs(rnorm(length(y), 0, 0.1^30))
      }
      if ((mean(y) + 3 * sd(y)) <= noiselevel) {
        y <- abs(rnorm(length(y), 0, 0.1^30))
      }
      decision <- "unknown"
    } else {
      # Apply a simple rule to take the first 20 percent and the last 15 percent
      # of any input data set to calculate the number of elements for the head 
      # (nh) and tail (nt), to deal with other data types like isothermal
      # amplifications
      
      nh <- trunc(length(y) * 0.2)
      nt <- trunc(length(y) * 0.15)
      if (t.test(head(y, n = nh), tail(y, n = nt), 
		  alternative = "less")$p.value > 0.01) {
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
        background = background)
  }
