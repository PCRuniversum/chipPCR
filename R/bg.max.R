bg.max <- function (x, y, bg.corr = 1.3, bg.start = 3, inder.approx = TRUE) {
  stop("Wrong classes of 'x'", call. = TRUE, domain = NA)
}

setGeneric("bg.max")

setMethod("bg.max", signature(x = "numeric", y = "numeric"), 
          function(x, y, bg.corr = 1.3, bg.start = 3, inder.approx = TRUE) {
            # Test if x and y have identical lengths.
            if (is.null(y)) 
              stop("Enter ordinate value")
            
            if (length(x) != length(y)) 
              stop("Use abscissa and ordinate data with same number of elements")
            
            calc.bg(x, y, bg.corr, bg.start, inder.approx)
          })

setMethod("bg.max", signature(x = "data.frame", y="missing"), 
          function(x, y, bg.corr = 1.3, bg.start = 3, inder.approx = TRUE) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            calc.bg(x[, 1], x[, 2], bg.corr, bg.start, inder.approx)
          })

setMethod("bg.max", signature(x = "matrix", y = "missing"), 
          function(x, y, bg.corr = 1.3, bg.start = 3, inder.approx = TRUE) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            calc.bg(x[, 1], x[, 2], bg.corr, bg.start, inder.approx)
          })

#workhorse function
calc.bg <- function(x, y, bg.corr, bg.start, inder.approx) {
  input <- data.frame(cyc = x, fluo = y)
  
  # Test if bg.corr is within a meaningful range.
  if (bg.corr < 1 || bg.corr > 8) 
    stop("bg.corr must be within 1 and 8.")
  
  # Test if bg.corr is within a meaningful range.
  if (bg.start < 0 || bg.start > length(x)) 
    stop("bg.start must be within 0 and the number of x values.")
  
  # Remove missing calues form y by using the fixNA function 
  # with the cubic spline method
  
  y <- fixNA(x, y, spline = TRUE)
  
  # Use Friedman's SuperSmoother to smooth the data
  # The smoothed data have less steep slopes in the 
  # transistion phase from linear to exponential
  yval.d <- supsmu(x, y, span = 0.09)$y
  
  if (inder.approx) {
    der <- inder(x, yval.d)
    deltax <- der[["x"]]
    deltax1 <- der[["x"]]
    delta <- der[["1st_der"]]
    delta1 <- der[["2nd_der"]]
  } else {
  # Form the derivatives of the smoothed data.
  # The maximum and the minimum of the seconde derivative
  # are the starting points to define the approximate 
  # start and the end of the exponential phase
    delta <- vector()
    deltax <- vector()
    for (i in 1L:(length(yval.d) - 1)) {
      delta.t <- yval.d[i + 1] - yval.d[i]
      delta <- c(delta, delta.t)
      delta.tx <- (x[i + 1] + x[i]) / 2
      deltax <- c(deltax, delta.tx)
    }
    
    delta1 <- vector() 
    deltax1 <- vector() 
    for (i in 1L:(length(delta) - 1)) { 
      delta.t <- delta[i + 1] - delta[i] 
      delta1 <- c(delta1, delta.t) 
      delta.tx <- (deltax[i + 1] + deltax[i]) / 2 
      deltax1 <- c(deltax1, delta.tx)
    }
  }
  
  d <- data.frame(deltax, delta) 
  d1 <- data.frame(deltax1, delta1)
  
  bg.stop <- trunc(d1[which(d1[, 2] == max(d1[, 2])), 1] - 
                     bg.corr * (d1[which(d1[, 2] == min(d1[, 2])), 1] - 
                                  d1[which(d1[, 2] == max(d1[, 2])), 1]), 0)
  
  
  amp.stop <- trunc(d1[which(d1[, 2] == min(d1[,  2])), 1] + 
                      bg.corr * (d1[which(d1[, 2] == min(d1[, 2])), 1] - 
                                   d[which(d[, 2] == max(d[, 2])), 1]), 0) 
  
  # Perform error handling on the the estimated start and end 
  # of the amplification process. Used hard coded values to prevent
  # to early or to late bg.start or bg.stop values.
  if (is.na(bg.stop)) {bg.stop <- round(length(yval.d) * 0.8)}
  if (bg.stop <= 9) {bg.stop <- 10}
  if ((bg.stop >= length(yval.d) * 0.6) || (is.na(bg.stop))) {
    bg.stop <- round(length(yval.d) * 0.6)
  }
  #threshold bg.max
  th.bg <- median(y[c(bg.stop - 1, bg.stop, bg.stop + 1)])  
  th <- quantile(y, 0.4)	#threshold qPCR background
  if (th.bg >= th) {bg.stop <- 10}
  fluo <- y[bg.stop]
  
  # Test if background range is meaningful.
  if (bg.stop <= bg.start) 
    stop("Start of background must be less than maximal 
		  background value.")
  new("bg", d = d, d1 = d1, delta = delta, delta1 = delta1, 
      bg.start = bg.start, bg.stop = bg.stop, 
      bg.corr = bg.corr, fluo = fluo, amp.stop = amp.stop,
      input = input)
  
}
