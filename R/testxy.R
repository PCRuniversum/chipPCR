testxy <- function(x, y) {
  # Test if x and y exist and have identical lengths.
  if (is.null(x)) 
    stop("Enter abscissa value")
  if (is.null(y)) 
    stop("Enter ordinate value")
#   if (is.numeric(x) ) 
#     stop("Abscissa value must have numeric class")
#   if (is.numeric(y)) 
#     stop("Ordinate value must have numeric class")
  if (length(x) != length(y)) 
    stop("Use abscissa and ordinate data with same number of 
  	elements")
}