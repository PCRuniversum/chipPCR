th.cyc <-
  function(x, y, r = 2500, auto = FALSE, linear = TRUE) {
    # Sanity test for input values
    testxy(x, y, length = FALSE)
    # Rearrange data for further processing
    
    xy <- data.frame(x = x, y = y)
    xy <- xy[!is.na(xy[["x"]]), ]
    
    # Determine type of threshold calculation
    r <- ifelse(auto, quantile(y[1L:10], 0.1) + 3 * mad(y[1L:10]), r)
    
    # Before runing the analysis, test if signal is indeed larger than the 
    # threshold.
    
<<<<<<< HEAD
#     if (quantile(xy[, 2], 0.98) <= r) {
#       # TODO: FIX OUTPUT
#       warning("Maximum of amplification signal lower than threshold ('r'). Cosider to lower 'r'.")
#     } else {
      # Actually used number of neighbours around the threshold value
      n <- seq(2, 8, 1)
      
      # List of all regression results for all tested regressions with different 
      # numbers of neighbours
      res.th.est <- lapply(n, function(n) 
        th.est(xy, r = r, n, linear = linear))
      
      # Results of the selection criterium R squared
      res.r.squ <- sapply(1L:length(n), function(i) 
        res.th.est[[i]][[1]][["r.squared"]])
=======
    #     if (quantile(xy[, 2], 0.9) <= r) {
    #       # TODO: FIX OUTPUT
    #       stop("Maximum of signal lower than threshold (r).")
    #     } else {
    # Actually used number of neighbours around the threshold value
    n <- seq(2, 8, 1)
    
    # List of all regression results for all tested regressions with different 
    # numbers of neighbours
    res.th.est <- lapply(n, function(n) 
      th.est(xy, r = r, n, linear = linear))
    
    # Results of the selection criterium R squared
    res.r.squ <- sapply(1L:length(n), function(i) 
      res.th.est[[i]][[1]][["r.squared"]])
    
    # Result of the optimal regression
    xy.sum <- res.th.est[[which.max(res.r.squ)]]
    
    if (linear == FALSE) {
      # Extract the coefficients of the regression.
      a <- xy.sum[[1]][["coefficients"]][3, 1]
      b <- xy.sum[[1]][["coefficients"]][2, 1]
      c <- xy.sum[[1]][["coefficients"]][1, 1]
>>>>>>> ff6c09593643e6bf90c3ecb8502344bd8e6dc85b
      
      # Calculate the exact Ct value at the user defined fluorescence threshold.
      # Use either the linear or quadratic model.
      
<<<<<<< HEAD
      if (linear == FALSE) {
        # Extract the coefficients of the regression.
        a <- xy.sum[[1]][["coefficients"]][3, 1]
        b <- xy.sum[[1]][["coefficients"]][2, 1]
        c <- xy.sum[[1]][["coefficients"]][1, 1]
        
        # Calculate the exact Cq (Ct value) at the user defined fluorescence threshold.
        # Use either the linear or quadratic model.
        
        sign <- inder(xy.sum[["values"]])
        switch.sign <- which.max(sign[, "d1y"])
        sqrt.delta <- sqrt(b^2 - 4*a*(c - r))
        if (sign[switch.sign, "y"] < r) {
          x.cal <- (-b - sqrt.delta)/(2*a)
        } else {
          x.cal <- (-b + sqrt.delta)/(2*a)
        }
=======
      sign <- inder(xy.sum[["values"]])
      switch.sign <- which.max(sign[, "d1y"])
      sqrt.delta <- sqrt(b^2 - 4*a*(c - r))
      if (sign[switch.sign, "y"] < r) {
        x.cal <- (-b - sqrt.delta)/(2*a)
>>>>>>> ff6c09593643e6bf90c3ecb8502344bd8e6dc85b
      } else {
        x.cal <- (-b + sqrt.delta)/(2*a)
      }
<<<<<<< HEAD
      
      # Test if the calculated Cq (Ct value) is larger than the maximum number of the cycles
      if (x.cal > max(y)) x.cal <- NA
      
      # Create the output fot the exact Cq (Ct value), the regression and the neighbours 
      # of the cycle and fluorescence values at the threshold fluorescence.
      
      res <-matrix(c(x.cal, r), ncol = 2)
      colnames(res) <- c("cyc.th", "atFluo")
      
      new("th", .Data = res, 
          stats = xy.sum[["summary"]], 
          input = data.matrix(xy.sum[["values"]]))
#       }
=======
    } else {
      m <- xy.sum[[1]][["coefficients"]][1, 1]
      n <- xy.sum[[1]][["coefficients"]][2, 1]
      x.cal <- (r - m) / n
    }
    
    # Create the output fot the exact Ct value, the regression and the neighbours 
    # of the cycle and fluorescence values at the threshold fluorescence.
    
    res <-matrix(c(x.cal, r), ncol = 2)
    colnames(res) <- c("cyc.th", "atFluo")
    
    new("th", .Data = res, 
        stats = xy.sum[["summary"]], 
        input = data.matrix(xy.sum[["values"]]))
    #     }
>>>>>>> ff6c09593643e6bf90c3ecb8502344bd8e6dc85b
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
  
  xy.lm <- if (linear == TRUE) {
    lm(xy.out[, 2] ~ xy.out[, 1])
  } else {
    lm(xy.out[, 2] ~ xy.out[, 1] + I(xy.out[, 1]^2))
  }
  
  
  # summary of statistical values of the fit
  list(summary = summary(xy.lm), values = xy.out)
}