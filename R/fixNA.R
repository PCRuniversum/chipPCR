fixNA <- function(x, y, spline = TRUE, verbose = FALSE) {
  stop("Wrong classes of 'x'", call. = TRUE, domain = NA)
}

setGeneric("fixNA")

setMethod("fixNA", signature(x = "numeric", y = "numeric"), 
          function(x, y, spline = TRUE, verbose = FALSE) {
            # Test if x and y have identical lengths.
            if (is.null(y)) 
              stop("Enter ordinate value")
            
            if (length(x) != length(y)) 
              stop("Use abscissa and ordinate data with same number of elements")
            
            fixNAwork(x, y, spline, verbose)
          })

setMethod("fixNA", signature(x = "data.frame", y="missing"), 
          function(x, y, spline = TRUE, verbose = FALSE) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            x[, 1], fixNAwork(x[, 1], x[, 2], spline, verbose)
          })

setMethod("fixNA", signature(x = "matrix", y = "missing"), 
          function(x, y, spline = TRUE, verbose = FALSE) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            fixNAwork(x[, 1], x[, 2], spline, verbose)
          })

#workhorse function
fixNAwork <- function(x, y, spline, verbose) {
  #	      Test if x and y exist and have identical lengths.
  if (is.null(x)) 
    stop("Enter abscissa value")
  if (is.null(y)) 
    stop("Enter ordinate value")
  if (length(x) != length(y)) 
    stop("Use abscissa and ordinate data with same number of elements")
  # Test if y contains somethings else than NAs
  #if(complete.cases(y) != is.na(y))
  #  warnings("Use ordinate contain non-number elements (e.g., strings)")

  #	      Number of missing values "nNA"
  nNA <- length(which(is.na(y) == TRUE))
  
  # Indicate if information about the number of missing
  # values is needed
  if (verbose) 
    print(paste(nNA, "missing value(s) imputed.", sep = " "))
  
  # If NAs are present in the data set, substitie them by
  # linear approximation or by splines
  if ((nNA > 0) && (class(try(approx(x, y, n = length(x)), 
                              silent = TRUE))!="try-error")) {
    if (!spline) y[which(is.na(y))] <- approx(x, y, 
                                              n = length(x))$y[which(is.na(y))]
    if (spline) y[which(is.na(y))] <- spline(x, y, 
                                             n = length(y))$y[which(is.na(y))]
  } else {
  # If imputation fails use 0 instead
    y[which(is.na(y))] <- 0
  }

#   if (length(which(is.na(y) == TRUE)) == 0)
#     y <- y
  y
}
