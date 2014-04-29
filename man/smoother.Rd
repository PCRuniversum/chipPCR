\name{smoother}
\alias{smoother}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for Several Smoother of Amplification Data
}
\description{
Smoother is a wrapper for several smoothing functions including LOWESS, Moving 
Average, Friedman's SuperSmoother, Cubic Spline and Savitzky-Golay smoothing 
filter for amplification curve data.
}
\usage{
smoother(x, y, trans = FALSE, bg.outliers = FALSE, spline = TRUE, 
	 method = "savgol", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{x}{
	      x values for smoothing
  }
    \item{y}{
	      y values for smoothing
  }
    \item{trans}{
	      perform a linear transformation based on the trend of the 
	      background range.
  }
    \item{bg.outliers}{
			logical parameter which indicates of outliers should 
			be removed from 
    background range.
  }
    \item{spline}{
	    is a logical operator which defines of missing values should be 
	    imputed by spline approximation. The default is linear a 
	    approximation.
  }
    \item{method}{
		    defines which smoothing method should be used. The 
		    Savitzky-Golay smoothing filter is the default smoother. 
		    Use "lowess" for LOWESS smoother (locally-weighted 
		    polynomial regression, "mova" for moving average, "savgol" 
		    for Savitzky-Golay smoothing filter, "smooth" for cubic 
		    spline smooth, "spline" for standard cubic spline smooth, 
		    or "supsmu" for Friedman's SuperSmoother.
  }
    \item{...}{
		  dot operator for diverse arguments of the smoothers (see 
		  details).
  }
}
\details{
Amplification curve data of experimental thermo-cyclers may deliver results 
which are hard to interpret due to noise and scatter. For data presentation it 
is often useful to smooth or filter the data prior to presentation. Smoothing 
and filtering are different approaches with a similar outcome to pre-process 
an input signal in order to make it available for an analysis step. Filtering 
uses methods of signal processing. They take a data input and apply a function 
to form an output. There are linear and non-linear filters. The most common 
example of a linear filter is the the moving average. A moving average filter 
replaces sequentially data points with the average of the neighbor data 
points. The average is calculated from a defined span ("window") of odd 
count (e.g., 3, 5). The average herein may also refer to the median, the 
geometric or exponential mean. Smoothing in contrast uses statistical 
approaches. Such approaches use for example local regression models (e.g., 
least squares estimate) or cubic splines. Splines apply non-parametric 
regression by local cubic polynomials between knot points. Other examples for 
smoothers include Savitzky-Golay smoothing filter, Friedman's SuperSmoother, 
and Whittaker smoother. Several methods were integrated in the \code{chipPCR} 
package. The function \code{\link[chipPCR]{smoother}} is a wrapper for smoother 
functions and filters commonly used to process amplification curve data. The 
\code{\link[chipPCR]{smoother}} function was enhanced by functionality of the 
\code{\link[chipPCR]{fixNA}} and \code{\link[chipPCR]{CPP}} functions. The 
parameter "lowess" for LOWESS smoother (locally-weighted polynomial 
regression) can be tuned by the parameters \code{f} and \code{iter} (see 
\code{\link[stats]{lowess}} for details). The parameter "mova" for moving 
average can be tuned by the parameter \code{movaww}. \code{movaww} is the 
window size used for the moving average (see \code{\link[stats]{filter}} for 
details). The parameter "savgol" for Savitzky-Golay smoothing filter can be 
tuned by the parameter \code{p} (see \code{\link[signal]{sgolayfilt}} for 
details). The parameter "smooth" for cubic spline smooth can be tuned by the 
parameter \code{df.fact}. A \code{df.fact} value of 1 will leave the raw data 
almost unaffected while a value 0.5 will smooth the curve considerably. For 
further details refer to the \code{\link[stats]{smooth.spline}} function. The 
parameter "spline" for standard cubic spline smooth has currently no 
additional parameter. The parameter "supsmu" for Friedman's SuperSmoother can 
be tuned by the parameter \code{span}. For further details refer to the 
\code{\link[stats]{supsmu}} function.

}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\examples{
# Results of different smoothers. A in-silico amplification was performed
# using the AmpSim function and different smoothers were applied. Optimally
# all smoothers should give the same result (which is not the case)).
tmp <- AmpSim(cyc = c(1:35), bl = 0)

plot(tmp, main = "In-silico real-time PCR\n Effect of Smoother", 
     xlab = "Cycles", ylab ="refMFI", ylim = c(0,1), pch = 20, 
     type = "b", lwd = 2)

legend(25, 0.8, c("Raw data", "savgol", "lowess", "mova 3", "mova 5", 
		  "smooth", "spline", "supsmu"), pch = 20, lwd = 2, 
		  col = c(1:8))

tmp.savgol <- smoother(tmp[, 1], tmp[, 2], method = "savgol")
tmp.lowess <- smoother(tmp[, 1], tmp[, 2], method = "lowess")
tmp.mova.3 <- smoother(tmp[, 1], tmp[, 2], method = "mova", 
			movaww = 3)
tmp.mova.5 <- smoother(tmp[, 1], tmp[, 2], method = "mova", 
			movaww = 5)
tmp.smooth <- smoother(tmp[, 1], tmp[, 2], method = "smooth")
tmp.spline <- smoother(tmp[, 1], tmp[, 2], method = "spline")
tmp.supsmu <- smoother(tmp[, 1], tmp[, 2], method = "supsmu")

lines(tmp[, 1], tmp.savgol, type = "b", pch = 20, lwd = 2, col = 2)
lines(tmp[, 1], tmp.lowess, type = "b", pch = 20, lwd = 2, col = 3)
lines(tmp[, 1], tmp.mova.3, type = "b", pch = 20, lwd = 2, col = 4)
lines(tmp[, 1], tmp.mova.5, type = "b", pch = 20, lwd = 2, col = 5)
lines(tmp[, 1], tmp.smooth, type = "b", pch = 20, lwd = 2, col = 6)
lines(tmp[, 1], tmp.spline, type = "b", pch = 20, lwd = 2, col = 7)
lines(tmp[, 1], tmp.supsmu, type = "b", pch = 20, lwd = 2, col = 8)

par(fig = c(0.15,0.6,0.45,0.99), new = TRUE)
plot(tmp, main = "", xlab = "Cycles", ylab ="refMFI", 
     pch = 20, xlim = c(14,20), ylim = c(0,0.45))

tmp.savgol <- smoother(tmp[, 1], tmp[, 2], method = "savgol")
tmp.lowess <- smoother(tmp[, 1], tmp[, 2], method = "lowess")
tmp.mova.3 <- smoother(tmp[, 1], tmp[, 2], method = "mova", 
			movaww = 3)
tmp.mova.5 <- smoother(tmp[, 1], tmp[, 2], method = "mova", 
			movaww = 5)
tmp.smooth <- smoother(tmp[, 1], tmp[, 2], method = "smooth")
tmp.spline <- smoother(tmp[, 1], tmp[, 2], method = "spline")
tmp.supsmu <- smoother(tmp[, 1], tmp[, 2], method = "supsmu")

lines(tmp[, 1], tmp.savgol, type = "b", pch = 20, lwd = 2, col = 2)
lines(tmp[, 1], tmp.lowess, type = "b", pch = 20, lwd = 2, col = 3)
lines(tmp[, 1], tmp.mova.3, type = "b", pch = 20, lwd = 2, col = 4)
lines(tmp[, 1], tmp.mova.5, type = "b", pch = 20, lwd = 2, col = 5)
lines(tmp[, 1], tmp.smooth, type = "b", pch = 20, lwd = 2, col = 6)
lines(tmp[, 1], tmp.spline, type = "b", pch = 20, lwd = 2, col = 7)
lines(tmp[, 1], tmp.supsmu, type = "b", pch = 20, lwd = 2, col = 8)


# Plot the difference of the smoothed / filtered data
# to the raw data against the cycles
# The largest error is in the transition phases between
# start and end of the detectable amplification process.
par(fig = c(0,1,0,0.65))
plot(NA, NA, type = "b", col = 2, pch = 20, xlim = c(1,35), 
      ylim = c(-0.1,0.1), xlab = "Cycle", 
      ylab = "delta refMFI (raw - smoothed)", 
     main = "Smoothed / Filtered data")

legend(1.5, 0.1, ncol = 2, c("savgol", "lowess", "mova 3", "mova 5", 
	"smooth", "spline", "supsmu"), pch = 20, lwd = 2, 
	col = c(2:8))

lines(tmp[, 1], tmp[, 2] - tmp.savgol, type = "b", col = 2, pch = 20)
lines(tmp[, 1], tmp[, 2] - tmp.lowess, type = "b", col = 3, pch = 20) 
lines(tmp[, 1], tmp[, 2] - tmp.mova.3, type = "b", col = 4, pch = 20) 
lines(tmp[, 1], tmp[, 2] - tmp.mova.5, type = "b", col = 5, pch = 20)
lines(tmp[, 1], tmp[, 2] - tmp.smooth, type = "b", col = 6, pch = 20)
lines(tmp[, 1], tmp[, 2] - tmp.spline, type = "b", col = 7, pch = 20)
lines(tmp[, 1], tmp[, 2] - tmp.supsmu, type = "b", col = 8, pch = 20)

par(fig = c(0,1,0.55,1), new = TRUE)

plot(tmp, type = "b", col = 1, pch = 20, xlab = "", ylab = "RFU", 
      main = "Raw data")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ smooth }
\keyword{ spline }
\keyword{ Friedman }
\keyword{ Savitzky-Golay }
