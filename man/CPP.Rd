\name{CPP}
\alias{CPP}
\alias{CPP.numeric}
\alias{CPP.matrix}
\alias{CPP.data.frame}
\alias{CPP,numeric,numeric-method}
\alias{CPP,matrix,missing-method}
\alias{CPP,data.frame,missing-method}

\title{
Curve Pre-Processor
}
\description{
The function \code{\link[chipPCR]{CPP}} encompasses a set of functions to 
pre-process an amplification curve. The pre-processing includes options to 
normalize curve data, to remove background, to remove outliers
in the background range and to test if an amplification is significant.
}
\usage{
\S4method{CPP}{numeric,numeric}(x, y, smoother = TRUE, trans = TRUE, 
	       rob.reg = "lmrob", bg.outliers = FALSE, median = FALSE, 
	       norm = "none", qnL = 0.1, amptest = FALSE, manual = FALSE, 
	       nl = 0.08)

\S4method{CPP}{matrix,missing}(x, y, smoother = TRUE, trans = TRUE, 
	       rob.reg = "lmrob", bg.outliers = FALSE, median = FALSE, 
	       norm = "none", qnL = 0.1, amptest = FALSE, manual = FALSE, 
	       nl = 0.08)

\S4method{CPP}{data.frame,missing}(x, y, smoother = TRUE, trans = TRUE, 
	       rob.reg = "lmrob", bg.outliers = FALSE, median = FALSE, 
	       norm = "none", qnL = 0.1, amptest = FALSE, manual = FALSE, 
	       nl = 0.08)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    is a vector containing the time or cycle values or a matrix/data frame
    containing both time or cycle values and fluorescence.
}
  \item{y}{
        is a vector containing the fluorescence values. Omitted if \code{x} 
        is a data frame/matrix.
}
  \item{smoother}{
	logical parameter which indicates if smoother should be used.
}
  \item{trans}{
	defines if the slope of the background range in a curve should be 
	corrected by a linear regression.
}
  \item{rob.reg}{
	defines the method ("rfit", "lmrob", "rq") for the robust linear regression.
}
  \item{bg.outliers}{
	is a logical argument which to remove outliers in the background range.
}
  \item{median}{
	If set to TRUE, median is used instead of mean in outlier replacement. 
	The mean is used by default.
}
  \item{norm}{
	is a argument to use a "none", "minmax", "lugn", or
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
}
\details{
The function \code{\link[chipPCR]{CPP}} uses the function 
\code{\link[chipPCR]{bg.max}} to estimate automatically the start of the 
amplification process. In the background range there is often noise which 
makes it harder to determine a meaningful background value. Therefore 
\code{\link[chipPCR]{CPP}} can optionally remove outliers by finding the 
value with largest difference from the mean as provided by the 
\code{\link[outliers]{rm.outlier}} function. The functions also tries to 
prevent calculations on non amplified signals. The parameter \code{qnL} is 
a user defined quantile which is used for the quantile normalization. A 
quantile normalization herein refers to an approach which is less prone to 
outliers than a normalization based on the minimum and the maximum of an 
amplification curve. The slope of the background range is often unequal to 
zero. By setting the parameter \code{trans} it is possible to apply a 
simple correction of the slope. Thereby either a robust linear regression 
by computing MM-type regression estimators, a nonparametric rank-based 
estimator  or a standard linear regression model. Care is needed when using 
\code{trans} with time series (see \code{\link[stats]{lm}} for details).
}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\seealso{ \code{\link{rq}}, \code{\link{rfit}}, \code{\link{lm}}}

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

# Use CPP to preprocess the data by removing the missing value and 
# normalization of the data
plot(NA, NA, xlab = "Time [sec]", ylab = "refMFI", 
     main = "Curve Pre-Processor Applied to HDA Data", 
     xlim = c(0, 2500), ylim = c(0,1.1), pch = 20)
for (i in 3:5) {
  y.cpp <- CPP(C17[2:50, 1], C17[2:50, i], norm = "minmax", 
	      bg.outliers = TRUE)$y.norm
  lines(C17[2:50, 1], y.cpp, col = i - 2, 
	type = "b", pch = 20)
}
legend(50, 1, c("55 deg Celsius", "60 deg Celsius", "65 deg Celsius"), 
	col = c(1,2,3), pch = rep(20,3))
par(mfrow = c(1,1))

# Example for dB analysis
T60 <- cbind(apply(VIMCFX96_60[, c(2:97)], 2, 
		    function(x) CPP(VIMCFX96_60[, 1], x)$dB))
T69 <- cbind(apply(VIMCFX96_69[, c(2:97)], 2, 
		    function(x) CPP(VIMCFX96_69[, 1], x)$dB))
		    
plot(T60, T69, xlab = "dB @ 60 deg. Celsius", 
      ylab = "dB @ 69 deg. Celsius")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ background }
\keyword{ noise }
\keyword{ outlier }
\keyword{ normalize }
\keyword{ amplification }
\keyword{ methods }
