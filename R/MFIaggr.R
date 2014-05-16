MFIaggr <- function(x, y, RSD = FALSE, rob = FALSE, llul = c(1,10)){
  
  #Define if "robust" or standard function should be used as measures
  #Test if x and y exist.
  testxy(x, y, length = FALSE)
  
  # Test if llul has only two values
  if (!is.null(llul) && length(llul) != 2)
    stop("Use two cycle values (e.g., llul = c(1,10)) to set 
        the range for the analysis.")
  
  # llul defines the range of interrest (ROI) to be analyzed
  # in detail. In particular, early cycles (background range)
  # and final cycles (plateau phase) are potential targets.
  llul <- sort(llul)	
  
  # Decide which method for the calculation of location and 
  # dispersion should be used. "rob" means robust and it will
  # use median and MAD instead of mean and sd
  
  if (rob) {
    loc.fct <- median
    dev.fct <- mad
  } else {
    loc.fct <- mean
    dev.fct <- sd
  }
  
  # NOTE: no error hanling in if only on collumn of input data is
  # used. Waring: apply will fail in this case. Need fix: yes, see
  # effcalc
  
  # Use apply over the input data to calculate the location and 
  # dispersion of a data bulk
  
  y.m <- apply(y, 1, loc.fct)
  y.sd <- apply(y, 1, dev.fct)
  
  # Decide if the relative standard deviation should be 
  # calculated
  
  if (RSD) {
    y.cv <- (y.sd / y.m) * 100
  } else {
    y.cv <- y.sd / y.m
  }
  
  # Apply the results to the data.frame "res" and label
  # collumns according to the used location and dispersion
  # method
  res <- data.frame(x, y.m, y.sd, y.cv)
  
  if (rob) {
    if(RSD) {
      names(res)  <- c("Cycle", "Location (Median)", 
                       "Deviation (MAD)", 
                       "Coefficient of Variance (RSD)")
    } else {
      names(res) <- c("Cycle", "Location (Median)", 
                      "Deviation (MAD)", 
                      "Coefficient of Variance (RSD [%])")
    }
  } else {
    if(RSD) {
      names(res)  <- c("Cycle", "Location (Mean)", 
                       "Deviation (SD)", 
                       "Coefficient of Variance (RSD)")
    } else {
      names(res)  <- c("Cycle", "Location (Mean)", 
                       "Deviation (SD)", 
                       "Coefficient of Variance (RSD [%])")
    }
  }
  
  # Calcuate robust und non-robust location and dispersion
  # parameters of the ROI and apply the results to stats
  
  stats <- c(round(mean(unlist(y[llul, ]), na.rm = TRUE), 3),
             round(median(unlist(y[llul, ]), na.rm = TRUE), 3), 
             round(sd(unlist(y[llul, ]), na.rm = TRUE), 2),
             round(mad(unlist(y[llul, ]), na.rm = TRUE), 2)
  )
  
  res.dens <- density(unlist(y[llul, ]))
  res.qq <- qqnorm(unlist(y[llul, ]), plot.it = FALSE)
  #res is the an object of the type data.frame containing the 
  #temperature, location, deviation and coefficient of variance.
  new("refMFI", .Data = res, density = res.dens, 
      qqnorm.data = y[llul, ], stats = stats)  
}
