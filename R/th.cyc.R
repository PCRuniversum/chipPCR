th.cyc <-
  function(x, y, r = 2500, auto = FALSE, linear = TRUE) {
    # Sanity test for input values
    testxy(x, y, length = FALSE)
    # Rearrange data for further processing
    xy <- data.frame(x = x, y = y)
    
    # Determin type of threshold calculation
    r <- ifelse(auto, quantile(y[1:10], 0.1) + 3 * mad(y[1:10]), r)
    
    # Actually used number of neighbours
    n <- seq(2, 8, 1)
    
    # List of all regression results for all tested regressions with different 
    # numbers of neighbours
    res.th.est <- lapply(n, function(n) {th.est(xy, r = r, n, linear = linear)})
    
    # Results of the selection criterium R squared
    res.r.squ <- sapply(1L:length(n), function(i) 
      res.th.est[[i]][[1]][["r.squared"]])
    
    # Result of the optimal regression
    xy.sum <- res.th.est[[which.max(res.r.squ)]]
    
    if (linear == FALSE) {
      # Extract the coefficients of the regression.
      a <- xy.sum[[1]][["coefficients"]][3,1]
      b <- xy.sum[[1]][["coefficients"]][2,1]
      c <- xy.sum[[1]][["coefficients"]][1,1]
      
      # Calculate the exact Ct value at the user defined fluorescence threshold.
      # Use either the linear or quadratic model.
      
      sign <- inder(xy.sum[["values"]])
      switch.sign <- which(sign[, "d1y"] == max(sign[, "d1y"]))
      if (sign[switch.sign, "y"] < r) {
        x.cal <- (-b/a)/2 - sqrt(((-b/a)/2)^2 - (c - r)/a)
      } else {
        x.cal <- (-b/a)/2 + sqrt(((-b/a)/2)^2 - (c - r)/a)
      }
    } else {
      m <- xy.sum[[1]][["coefficients"]][1, 1]
      n <- xy.sum[[1]][["coefficients"]][2, 1]
      x.cal <- (r - n) / m
    }
    
    # Create the output fot the exact Ct value, the regression and the neighbours 
    # of the cycle and fluorescence values at the threshold fluorescence.
    
    out <- list(Results = rbind(cyc.th = x.cal, atFluo = r), 
                stats = xy.sum[["summary"]], 
                Input = xy.sum[["values"]])
    out
  }


# Helper function to determine the number of neighbours for the regression
th.est <- function(xy, r, n, linear) {
  # Fetch the neighbours of the cycle and fluorescence values at the threshold
  # fluorescence.
  
  xy.out <- rbind(tail(xy[xy[, 2] <= r, ], n),
                  head(xy[xy[, 2] >= r, ], n)
  )
  
  # Determine with a quadratic polynomial the equation for the neighbours of the 
  # cycle and fluorescence values at the threshold fluorescence.
  
  if (linear == TRUE) {
    xy.lm <- lm(xy.out[, 2] ~ xy.out[, 1])
  } else {
    xy.lm <- lm(xy.out[, 2] ~ xy.out[, 1] + I(xy.out[, 1]^2))
  }
  
  
  # summary of statistical values of the fit
  out <- list(summary = summary(xy.lm), values = xy.out)
}