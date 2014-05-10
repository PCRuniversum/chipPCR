amptester <-
  function(y, manual = FALSE, noiselevel = 0.08, background = NULL) {
    # Test if y exist.
    if (is.null(y)) 
      stop("Enter y value")
    # Test if background has only two values
    if (!is.null(background) && length(background) != 2)
      stop("Use only two values (e.g., background = c(1,10)) 
		to set the range for the background correction")
    if (is.null(background) && manual)
      stop("Manual test requires specified background.") 
    
    #if background is NULL, sorting it is pointless and invokes warning
    if (!is.null(background))
      background <- as.integer(sort(background))
    
    y <- fixNA(1L:length(y), y)
    
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
      if (t.test(head(y), tail(y), alternative = "less")$p.value > 0.01) {
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
