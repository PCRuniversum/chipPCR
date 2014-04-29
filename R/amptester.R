amptester <-
function (y, manual = FALSE, noiselevel = 0.08, background = NULL) {
	tmp.warn <- getOption("warn")
  	options(warn = -1)
  	# Test if y exist.
  	if (is.null(y)) 
  	  stop("Enter y value")
  	# Test if background has only two values
  	if (!is.null(background) && length(background) != 2)
  	  stop("Use only two values (e.g., background = c(1,10)) 
		to set the range for the background correction")
  	if (is.null(background) && manual)
  	  stop("Manual test requires specified background.") 
    
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
  } else (
      if (t.test(head(y), tail(y), alternative = "less")$p.value > 0.01) {
	y <- abs(rnorm(length(y), 0, 0.1^30))
	print("negative")
      } else print("positive")
    )
    options(warn = tmp.warn)
    y
}
