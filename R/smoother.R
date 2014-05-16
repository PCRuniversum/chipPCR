smoother <- function(x, y, trans = FALSE, bg.outliers = FALSE, 
		     method = "savgol", CPP = TRUE, ...) {
  # Determine the time/cycle resolution of the data
  testxy(x, y)
  
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
  
  #recognize method
  #possible methods
  pos.meth <- c("lowess", "mova", "savgol", "smooth", 
                "spline", "supsmu", "whit1", "whit2")
  method <- unname(sapply(method, tolower))
  
  #uniformize names
  if (length(method) != 1 || method != "all") {
    for (i in pos.meth) 
      for (j in 1L:length(method))
        if (any(grepl(method[j], i)))
          method[j] <- i
    
    #check for presence of invalid names
    invalids <- !(method %in% pos.meth)
    if (sum(invalids) > 0)
      stop(paste0("Invalid method(s) chosen: ", paste0(method[invalids], 
                                                       collapse = ", ")))
  } else {
    method <- pos.meth
  }
  
  names(method) <- method
  ######TODO##############################################
  # 	ADD checks fro proper use of the filters and the smoother!
  #     # Test if window size of moving average is correct
  #     if (movaww <= 1 || movaww > 10) 
  #       stop("Enter movaww value between 1 and 10")
  #     # The test of movaww should be dependent on the number of
  #     # of elements. For example, movaww of 10 and 35 elements will 
  #     # be a bad smoother, but movaww of 10 and 350 elements will be 
  #     # ok. Proposels: either make it emerical (define ranges) or test
  #     # the shift of the first derivative maximumvalue
  ######/TODO##############################################
  
  # impute missing values by linear approximation in "y" and substitute 
  # them in "y.tmp"
  if(any(is.na(y))) {
    y.tmp <- fixNA(x, y, spline = TRUE)
  } else {
    y.tmp <- y
  }
  
  all.smooths <- lapply(1L:length(method), function(i) {
    y.tmp <- switch(method[i],
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
                    spline = do.call(function(x, y, n = length(y.tmp)) {
                      spline(x, y, n = n)[["y"]]
                    }, c(list(x = x, y = y.tmp), ...)),
                    supsmu = do.call(function(x, y, span = 0.01)
                      supsmu(x = x, y = y, span = span)
                      , c(list(x = x, y = y.tmp), ...))[["y"]],
                    whit1 = do.call(function(y, lambda = 0.01)
                      whit1(y = y, lambda = lambda)
                      , c(list(y = y.tmp), ...)),
                    whit2 = do.call(function(y, lambda = 0.01)
                      whit2(y = y, lambda = lambda)
                      , c(list(y = y.tmp), ...))
    )
    
    # Invoke the CPP function to perform a preprocessing of the 
    # smoothed data
    # TODO: check if there are potential problems related to the
    # bg.max function which is used by CPP
    if (CPP) {
      tmp.CPP  <- CPP(x = x, y = y.tmp, trans = trans, 
		      bg.outliers = bg.outliers)
      
      # Do output of the smoothed data
      tmp.CPP$y.norm
    } else {
	y.tmp
      }
   y.tmp
  })
  
  #attr(y.norm, "method") <- method
  res <- do.call(cbind, all.smooths)
  colnames(res) <- method
  res
}

setGeneric("smoother")



setMethod("smoother", signature(x = "data.frame", y="missing"), 
          function(x, y, trans = FALSE, bg.outliers = FALSE, 
                   method = "savgol", CPP = TRUE, ...) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            smoother(x[,1], x[,2], trans, bg.outliers, spline, method, ...)
          })

setMethod("smoother", signature(x = "matrix", y = "missing"), 
          function(x, y, trans = FALSE, bg.outliers = FALSE, 
                   method = "savgol", CPP = TRUE, ...) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            smoother(x[,1], x[,2], trans, bg.outliers, spline, method, ...)
          })



