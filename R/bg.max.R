bg.max <-
  function (x, y, bg.corr = 1.3, bg.start = 3, verbose = FALSE) {
    
    # Test if x and y exist and have identical lengths.
    if (is.null(x)) 
      stop("Enter abscissa value")
    
    if (is.null(y)) 
      stop("Enter ordinate value")
    
    if (length(x) != length(y)) 
      stop("Use abscissa and ordinate data with same number of 
		 elements")
    
    # Test if bg.corr is within a meaningful range.
    if (bg.corr < 1 || bg.corr > 8) 
      stop("bg.corr must be within 1 and 8.")
    
    # Test if bg.corr is within a meaningful range.
    if (bg.start < 0 || bg.start > length(x)) 
      stop("bg.start must be within 0 and the number of x values.")
    
    y <- fixNA(x, y, spline = TRUE)
    
    yval.d <- supsmu(x, y, span = 0.09)$y
    delta <- vector()
    deltax <- vector()
    for (i in 1L:(length(yval.d) - 1)) {
      delta.t <- yval.d[i+1] - yval.d[i]
      delta <- c(delta, delta.t)
      delta.tx <- (x[i+1] + x[i]) / 2
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
    
    d <- data.frame(deltax, delta) 
    d1 <- data.frame(deltax1, delta1)
    
    bg.stop <- trunc(d1[which(d1[, 2] == max(d1[, 2])), 1] - 
                       bg.corr * (d1[which(d1[, 2] == min(d1[, 2])), 1] - 
                                    d1[which(d1[, 2] == max(d1[, 2])), 1]), 0)
    
    
    amp.stop <- trunc(d1[which(d1[, 2] == min(d1[,  2])), 1] + 
                        bg.corr * (d1[which(d1[, 2] == min(d1[, 2])), 1] - 
                                     d[which(d[, 2] == max(d[, 2])), 1]), 0) 
    
    if (is.na(bg.stop)) {bg.stop <- round(length(yval.d) * 0.8)}
    if (bg.stop <= 9) {bg.stop <- 10}
    if ((bg.stop >= length(yval.d) * 0.6) || (is.na(bg.stop))) {
      bg.stop <- round(length(yval.d)*0.6)
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
        bg.corr = bg.corr, fluo = fluo, amp.stop = amp.stop)
    
  }
