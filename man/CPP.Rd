\name{CPP}
\alias{CPP}
\alias{CPP.numeric}
\alias{CPP.matrix}
\alias{CPP.data.frame}
\alias{CPP,numeric,numeric-method}
\alias{CPP,matrix,missing-method}
\alias{CPP,data.frame,missing-method}

\title{
Curve Pre-processor
}

\description{
The function \code{\link[chipPCR]{CPP}} encompasses a set of functions to 
pre-process an amplification curve. The pre-processing includes options to 
normalize curve data, to remove background, to remove outliers
in the background range and to test if an amplification is significant.
}
\usage{
\S4method{CPP}{numeric,numeric}(x, y, smoother = TRUE, method = "savgol", 
				trans = FALSE, method.reg = "lmrob", 
				bg.outliers = FALSE, median = FALSE, 
				method.norm = "none", qnL = 0.03, amptest = FALSE, 
				manual = FALSE, nl = 0.08, bg.range = NULL, ...)

\S4method{CPP}{matrix,missing}(x, y, smoother = TRUE, method = "savgol", 
			       trans = FALSE, method.reg = "lmrob", 
			       bg.outliers = FALSE, median = FALSE, 
			       method.norm = "none", qnL = 0.03, amptest = FALSE, 
			       manual = FALSE, nl = 0.08, bg.range = NULL, ...)

\S4method{CPP}{data.frame,missing}(x, y, smoother = TRUE, 
				   method = "savgol", trans = FALSE, 
				   method.reg = "lmrob", bg.outliers = FALSE, 
				   median = FALSE, method.norm = "none", 
				   qnL = 0.03, amptest = FALSE, 
				   manual = FALSE, nl = 0.08, bg.range = NULL, ...)
}

\arguments{
  \item{x}{
    is a vector containing the time or cycle values or a matrix or data frame
    containing both time or cycle values and fluorescence.
}
  \item{y}{
        is a vector containing the fluorescence values. Omitted if 
\code{x} 
        is a data frame or matrix.
}
  \item{smoother}{
	logical parameter which indicates if smoother should be used.
}

  \item{method}{
		a vector of names defining which smoothing method 
should be used. The Savitzky-Golay smoothing filter is the default 
smoother. Use "lowess" for LOWESS smoother (locally-weighted polynomial 
regression), "mova" for moving average, "savgol" for Savitzky-Golay 
smoothing filter, "smooth" for cubic spline smooth, "spline" for standard 
cubic spline smooth, "supsmu" for Friedman's SuperSmoother, "whit1" for 
weighted Whittaker smoothing with a first order finite difference penalty, 
"whit2" for weighted Whittaker smoothing with a second order finite 
difference penalty or "all" for all implemented smoothing algorithms. 
}

  \item{trans}{
	defines if the slope of the background range in a curve should be 
	corrected by a linear regression.
}
  \item{method.reg}{
	defines the method ("rfit", "lmrob", "rq") for the robust linear 
regression. If equal to "least", \code{CPP} uses linear regression.
}
  \item{bg.outliers}{
	is a logical argument which to remove outliers in the background 
range.
}
  \item{median}{
	If set to TRUE, median is used instead of mean in outlier 
replacement.  The mean is used by default.
}
  \item{method.norm}{
	is a argument to use a "none", "minm", "max", "lugn", or
	"zscore" normalization.
}
  \item{qnL}{
	is the quantile to be used for the quantile normalization.
}
  \item{amptest}{
	is a logical operator which is used to set a test for a positive 
	amplification.
}
  \item{manual}{
	is used to test for a fixed threshold value of the background.
}
  \item{nl}{
	is a value used as fixed threshold value for the background.
}
  \item{bg.range}{
  is a \code{numeric} vector of length 2 pointing the background range.
  If \code{NULL}, the background range is calculated by \code{\link{bg.max}} function.
}
    \item{...}{
	dot operator for diverse arguments of 
	\code{\link[chipPCR]{smoother}} for details).
  }
}

\details{
The function \code{\link[chipPCR]{CPP}} uses the function 
\code{\link[chipPCR]{bg.max}} to estimate automatically the start of the 
amplification process. In the background range there is often noise which 
makes it harder to determine a meaningful background value. Therefore 
\code{\link[chipPCR]{CPP}} can optionally remove outliers by finding the 
value with largest difference from the mean as provided by the 
\code{\link[outliers]{rm.outlier}} function. The functions also tries to 
prevent calculations of non amplified signals. 

The slope of the background range is often unequal to 
zero. By setting the parameter \code{trans} it is possible to apply a 
simple correction of the slope. Thereby either a robust linear regression 
by computing MM-type regression estimators, a nonparametric rank-based 
estimator or a standard linear regression model. Care is needed when 
using 
\code{trans} with time series (see \code{\link[stats]{lm}} for details).

}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\seealso{ 
Normalization: \code{\link{normalizer}}

Smoothing: \code{\link{smoother}}

Robust linear regression: \code{\link{lm.coefs}}
}

\examples{
# Function to pre-process an amplification curve.
# Take a subset of the C17 data frame.
data(C17)
par(mfrow = c(2,1))
plot(NA, NA, xlab = "Time [sec]", ylab = "refMFI", 
     main = "HDA Raw Data", 
     xlim = c(0, 2500), ylim = c(0,1.1), pch = 20)
for (i in 3:5) {
  lines(C17[1:50, 1], C17[1:50, i], col = i - 2, 
	type = "b", pch = 20)
}

legend(50, 0.5, c("55 deg Celsius", "60 deg Celsius", "65 deg Celsius"), 
	col = c(1,2,3), pch = rep(20,3))

# Use CPP to pre-process the data by removing the missing value and 
# normalization of the data
plot(NA, NA, xlab = "Time [sec]", ylab = "refMFI", 
     main = "Curve Pre-processor Applied to HDA Data", 
     xlim = c(0, 2500), ylim = c(0,1.1), pch = 20)
for (i in 3:5) {
  y.cpp <- CPP(C17[2:50, 1], C17[2:50, i], method.norm = "minm", 
	      bg.outliers = TRUE)$y.norm
  lines(C17[2:50, 1], y.cpp, col = i - 2, 
	type = "b", pch = 20)
}
legend(50, 1, c("55 deg Celsius", "60 deg Celsius", "65 deg Celsius"), 
	col = c(1,2,3), pch = rep(20,3))
par(mfrow = c(1,1))

# Example for dB analysis
# qPCR analysis using the VIMCFX96_60 and the VIMCFX96_69 data sets from a 
# 96-well plate cycler (Bio-Rad CFX96, EvaGreen detection) experiment. 
# The dB values during the annealing (60 Celsius) and elongation (69 Celsius) 
# phase were determined by calling dB from CPP. The annealing phase values 
# were plotted against the elongation phase values and analyzed by 
# linear regression (lm, stats) and correlation test (cor.test, stats). Next, we 
# computed the interquartile range using IQR (stats) with the default settings. 
# The IQR at 60 Celsius was ~ 0.28 dB and ~ 0.21 dB at 69 Celsius.

par(mfrow = c(1,2), las = 0, bty = "n", cex.axis = 1.5, cex.lab = 1.5, 
     font = 2, cex.main = 1.1, oma = c(1,1,1,1))

pointer <- function (x, pos = 1, w = 5){
	    xx <- pos + rep(seq(-0.1, 0.1, length.out = w), 
				  ceiling(length(x)/w))
	    yy <- sort(x)
	    points(xx[1:length(yy)], yy, pch = 19)
	    points(pos, mean(x, na.rm = TRUE), col = 2, pch = 15, cex = 1.5)
	  }
	  
T60 <- cbind(apply(VIMCFX96_60[, c(2L:97)], 2, 
		    function(x) CPP(VIMCFX96_60[, 1], x, qnL = 0.03)$dB))
T69 <- cbind(apply(VIMCFX96_69[, c(2L:97)], 2, 
		    function(x) CPP(VIMCFX96_69[, 1], x, qnL = 0.03)$dB))
		    
res <- list(T60 = T60, T69 = T69)

plot(T60, T69, xlab = "dB @ 60 deg. Celsius", 
      ylab = "dB @ 69 deg. Celsius")
mtext("A", cex = 2, side = 3, adj = 0, font = 2) 
res.lm <- lm(T69 ~ T60)
abline(res.lm)

summary(res.lm)
cor.test(T60, T69)

boxplot(res, ylab = "dB", ylim = c(1.45, 3.5))
  pointer(T60, 1, 6)
  pointer(T69, 2, 6)
  
mtext("B", cex = 2, side = 3, adj = 0, font = 2) 

wcx <- wilcox.test(T60, T69)
lines(c(1,2), c(3.2,3.2))
text(1.5, 3.3, paste("p :", wcx$p.value))

IQR(T60)
IQR(T69)
par(mfrow = c(1,1))
}

\keyword{ background }
\keyword{ noise }
\keyword{ outlier }
\keyword{ normalize }
\keyword{ amplification }
\keyword{ methods }
