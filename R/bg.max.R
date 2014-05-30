bg.max <- function(x, y, bg.corr = 1.3, bg.start = 2, inder.approx = TRUE) {
  testxy(x, y)
  
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
  
  # Form the derivatives of the smoothed data.
  # The maximum and the minimum of the seconde derivative
  # are the starting points to define the approximate 
  # start and the end of the exponential phase
  
  if (inder.approx) {
    der <- inder(x, y, smooth.method = "supsmu")
    
  } else {
    sp <- smooth.spline(x, y)
    
    d1 <- predict(sp, x, 1)
    d2 <- predict(sp, x, 2)
    dat <- cbind(x, y, d1[["y"]], d2[["y"]])
    colnames(dat) <- c("x", "y", "d1y", "d2y")
    der <- new("der", '.Data' = dat, 'method' = "smooth.spline")
  }
  
  vals <- summary(der, print = FALSE)
  
  bg.stop <- trunc(vals[["SDM"]] - bg.corr * (vals[["SDm"]] - vals[["SDM"]]), 0)
  
  
  amp.stop <- trunc(vals[["SDm"]] + bg.corr * (vals[["SDm"]] - vals[["SDM"]]), 0) 
  
  #handle unrealistic values of bg.stop
  bg.stop <- ifelse(bg.stop < bg.start, NA, bg.stop)
  
  
  # Perform error handling on the the estimated start and end 
  # of the amplification process. Used hard coded values to prevent
  # to early or to late bg.start or bg.stop values.
  if (is.na(bg.stop)) 
    bg.stop <- round(length(y) * 0.8)
  if (bg.stop <= 9) 
    bg.stop <- 10
  if ((bg.stop >= length(y) * 0.6) || (is.na(bg.stop))) 
    bg.stop <- round(length(y) * 0.6)
  
  #handle unrealistic values of amp.stop
  amp.stop <- ifelse(amp.stop < bg.stop, NA, amp.stop)
  if(is.na(amp.stop))
     amp.stop <- length(y)
  
  #threshold bg.max
  th.bg <- median(y[c(bg.stop - 1, bg.stop, bg.stop + 1)])  
  th <- quantile(y, 0.4)  #threshold qPCR background
  if (th.bg >= th) {bg.stop <- 10}
  fluo <- y[bg.stop]
  
  # Test if background range is meaningful.
  if (bg.stop <= bg.start) 
    stop("Start of background must be less than maximal 
		  background value.")
  new("bg", .Data = slot(der, ".Data"),
      bg.start = bg.start, bg.stop = bg.stop, 
      bg.corr = bg.corr, fluo = fluo, amp.stop = amp.stop)
}
setGeneric("bg.max")


setMethod("bg.max", signature(x = "data.frame", y="missing"), 
          function(x, y, bg.corr = 1.3, bg.start = 2, 
                   inder.approx = TRUE) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            bg.max(x[, 1], x[, 2], bg.corr, bg.start, inder.approx)
          })

setMethod("bg.max", signature(x = "matrix", y = "missing"), 
          function(x, y, bg.corr = 1.3, bg.start = 2, 
                   inder.approx = TRUE) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            bg.max(x[, 1], x[, 2], bg.corr, bg.start, inder.approx)
          })

