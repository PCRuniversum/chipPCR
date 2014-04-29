fixNA <- function(x, y, spline = TRUE, verbose = FALSE) {
    tmp.warn <- getOption("warn")
    options(warn = -1)
#	      Test if x and y exist and have identical lengths.
    if (is.null(x)) 
    stop("Enter abscissa value")
    if (is.null(y)) 
    stop("Enter ordinate value")
    if (length(x) != length(y)) 
    stop("Use abscissa and ordinate data with same number of elements")
#	      Number of missing values "nNA"
    nNA <- length(which(is.na(y) == TRUE))
    if (verbose) 
    print(paste(nNA, "missing value(s) imputed.", sep = " "))
    
    if ((nNA > 0) && (class(try(approx(x, y, n = length(x)), 
		  silent = TRUE))!="try-error")) {
      if (!spline) y[which(is.na(y))] <- approx(x, y, 
					  n = length(x))$y[which(is.na(y))]
      if (spline) y[which(is.na(y))] <- spline(x,y, 
					  n = length(y))$y[which(is.na(y))]
      } else {
	y[which(is.na(y))] <- 0
	}
      options(warn = tmp.warn)
      if (length(which(is.na(y) == TRUE)) == 0) y <- y
}