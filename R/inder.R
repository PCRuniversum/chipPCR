#point f'(x_i)
first_midpoint <- function(y, h)
  1/12/h * (y[1] - 8 * y[2] + 8 * y[4] - y[5])

#point f'(x_0)
first_beginpoint0 <- function(y, h)
  1/12/h * (-25 * y[1] + 48 * y[2] - 36 * y[3] + 16 * y[4] - 3 * y[5])

#point f'(x_1)
first_beginpoint1 <- function(y, h)
  1/12/h * (-3 * y[1] - 10 * y[2] + 18 * y[3] - 6 * y[4] + y[5])

#point f'(x_{n-1})
first_endpoint1 <- function(y, h)
  - first_beginpoint1(rev(y), h)

#point f'(x_n)
first_endpoint0 <- function(y, h)
   - first_beginpoint0(rev(y), h)


#point f''(x_i)
sec_midpoint <- function(y, h)
  1/12/h^2 * (- y[1] + 16 * y[2] - 30 * y[3]  +  16 * y[4] - y[5])

#point f''(x_0)
sec_beginpoint0 <- function(y, h)
  1/12/h^2 * (35 * y[1] - 104 * y[2] + 114 * y[3]  -  56 * y[4] + 11 * y[5])

#point f''(x_1)
sec_beginpoint1 <- function(y, h)
  1/12/h^2 * (11 * y[1] - 20 * y[2] + 6 * y[3]  +  4 * y[4] - y[5])

#point f''(x_{n-1})
sec_endpoint1 <- function(y, h)
  sec_beginpoint1(rev(y), h)

#point f''(x_n)
sec_endpoint0 <- function(y, h)
  sec_beginpoint0(rev(y), h)


inder <- function(x, y, Nip = 4, logy = FALSE) {
  
  if (length(x) != length(y)) 
    stop("Use x and y vectors with same number of elements")
  
  if (Nip < 1) 
     stop("Use Nip equal or larger to 1")
  
  if (Nip > 10) 
     warning("Nip larger than 10 may case over-fitting")
  
  tmp.xy <- spline(x, y, n = Nip * length(x))
  
  x <- tmp.xy[["x"]]
  y <- tmp.xy[["y"]]
  
  if (logy == TRUE) y <- log10(tmp.xy[["y"]])
  
  #calculate h, naive approach
  h <- vapply(2L:length(x), function(i) x[i] - x[i - 1], 0)
  #instead of zero, in statement should be the minina
  if (var(h) > .Machine[["double.eps"]]) 
    warning("Points are not equidistant. The results of interpolation 
	      could be not correct.")
  
  h <- h[1]
  #calculate midpoints
  first_der <- c(first_beginpoint0(y[1:5], h),
                 first_beginpoint1(y[1:5], h),
                 vapply(3L:(length(y) - 2), function(i) first_midpoint(y[(i - 2):(i + 2)], h), 0),
                 first_endpoint1(y[(length(y) - 5):length(y)], h),
                 first_endpoint0(y[(length(y) - 5):length(y)], h))
  
  sec_der <- c(sec_beginpoint0(y[1:5], h),
               sec_beginpoint1(y[1:5], h),
               vapply(3L:(length(y) - 2), function(i) sec_midpoint(y[(i - 2):(i + 2)], h), 0),
               sec_endpoint1(y[(length(y) - 5):length(y)], h),
               sec_endpoint0(y[(length(y) - 5):length(y)], h))
                 
  dat <- cbind(x, y, first_der, sec_der)
  colnames(dat) <- c("x", "y", "d1y", "d2y")
  new("der", '.Data' = dat, 'method' = "spline")
}

setGeneric("inder")


setMethod("inder", signature(x = "data.frame", y="missing"), 
          function(x, y, Nip = 4, logy = FALSE) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            inder(x[, 1], x[, 2], Nip = Nip, logy = logy)
          })

setMethod("inder", signature(x = "matrix", y = "missing"), 
          function(x, y, Nip = 4, logy = FALSE) { 
            if (ncol(x) != 2) 
              stop("'x' must have two columns.")
            inder(x[, 1], x[, 2], Nip = Nip, logy = logy)
          })
