smoother <-
  function (x, y, trans = FALSE, bg.outliers = FALSE, spline = TRUE, 
	    method = "savgol", ...) { 
    tmp.warn <- getOption("warn")
    options(warn = -1)
    # Test if x and y exist and have identical lengths.
    if (is.null(x)) 
      stop("Enter abscissa value")
    if (is.null(y)) 
      stop("Enter ordinate value")
    if (length(x) != length(y)) 
      stop("Use abscissa and ordinate data with same number of elements")
      
    # Determine the time/cycle resolution of the data
    deltaCyc <- vector()
    for (i in 1L:(length(x) - 1)){
      tmp <- abs(x[i] - x[i + 1])
      deltaCyc <- c(deltaCyc,tmp)
      }
    # Code taken from integer {base} example section
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    if (is.wholenumber(mean(deltaCyc)) == FALSE)
      warning("Not equidistant measurement! smoother might not 
	       work properly.")
    
    #  recognize method
    method <- tolower(method)
    if (grepl(method, "lowess"))
      method <- "lowess"
    if (grepl(method, "mova")) 
      method <- "mova"
    if (grepl(method, "savgol"))
      method <- "savgol"
    if (grepl(method, "smooth"))
      method <- "smooth"
    if (grepl(method, "spline"))
      method <- "spline"
    if (grepl(method, "supsmu"))
      method <- "supsmu"
    
    if (!(method %in% c("lowess", "mova", "savgol", "smooth", "spline", 
			"supsmu")))
      stop("Invalid method chosen.")
      
######TODO##############################################
# 	ADD checks fro proper use of the filters and the smoother!
#     # Test if window size of moving average is correct
#     if (movaww <= 1 || movaww > 10) 
#       stop("Enter movaww value between 1 and 10")
######/TODO##############################################

    # impute missing values by linear approximation in "y" and substitute 
    # them in "y.tmp"
    y.tmp <- fixNA(x,y, spline = spline)
    
    y.tmp <- switch(method,
	    lowess = do.call(function(x, y, f = 0.01, iter = 3)
	      lowess(x = x, y = y, f = f, iter = iter)
			      , c(list(x = x, y = y.tmp), ...))[["y"]],
	    mova = do.call(function(x, movaww = 3)
	      as.vector(stats::filter(x, filter = rep(1/movaww, movaww), 
		    method = "convolution", sides = 2)), 
			    c(list(x = y.tmp), ...)),
	    savgol = do.call(function(y, p = 3)
	      sgolayfilt(x = y, p = p)
			      , c(list(y = y.tmp), ...)),
	    smooth = do.call(function(x, y, df.fact = 0.95) {
	      df.tmp <- data.frame(smooth.spline(x, y)[10])
	      smooth.spline(x, y, df =  (df.tmp * df.fact))[["y"]]
	    }, c(list(x = x, y = y.tmp), ...)),
	    spline = do.call(function(x, y) {
	      spline(x, y, n = length(y.tmp))[["y"]]
	    }, c(list(x = x, y = y.tmp))),
	    supsmu = do.call(function(x, y, span = 0.01)
	      supsmu(x = x, y = y, span = span)
			      , c(list(x = x, y = y.tmp), ...))[["y"]]
    )
    
    tmp.CPP  <- CPP(x = x, y = y.tmp, trans = trans, 
		    bg.outliers = bg.outliers)
    options(warn = tmp.warn)
    y.norm <- tmp.CPP$y.norm
    attr(y.norm, "method") <- method
    y.norm
  }