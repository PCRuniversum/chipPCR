\name{bg.max}
\alias{bg.max}
\alias{bg.max.numeric}
\alias{bg.max.matrix}
\alias{bg.max.data.frame}
\alias{bg.max,numeric,numeric-method}
\alias{bg.max,matrix,missing-method}
\alias{bg.max,data.frame,missing-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple function to detect and correct the background range
}
\description{
The function \code{\link[chipPCR]{bg.max}} is intended to detect and correct 
background noise. The detection is made without any assumptions regarding the 
model of the function.
}
\usage{
\S4method{bg.max}{numeric,numeric}(x, y, bg.corr = 1.3, bg.start = 1, inder.approx = TRUE)

\S4method{bg.max}{matrix,missing}(x, y, bg.corr = 1.3, bg.start = 1, inder.approx = TRUE)

\S4method{bg.max}{data.frame,missing}(x, y, bg.corr = 1.3, bg.start = 1, inder.approx = TRUE)
}

\arguments{
  \item{x}{
    is a vector containing the time or cycle values or data frame/matrix containing 
    cycle in the first column and fluorescence values in the second column. 
}
  \item{y}{
    is a vector containing the fluorescence values. Used only if \code{x} is 
    also a vector. 
}
  \item{bg.corr}{
  	a value which helps to tweak on the suggested background value of 
    \code{\link[chipPCR]{bg.max}}.
}
  \item{bg.start}{
	  a user defined value for the start of the background range.
}
  \item{inder.approx}{
    a \code{logical} value defining if data should be numerically derived by
    \code{\link[chipPCR]{inder}} function. If \code{FALSE}, derivative is calculated by the
    finite difference method.
}
}
\details{
Background range herein refers to a level of fluorescence measured before any 
specific amplification is detectable. The raw data (e.g., fluorescence 
intensity) measured after each step (cycle or time point) follow a non-linear 
progress. The background is assumed to be constant for the entire measurement. 
The algorithm of \code{\link[chipPCR]{bg.max}} is based on the assumption 
that the signal difference of successive cycles in the linear ground phase is 
approximately constant. After transition in the early exponential phase the 
signal changes drastically. First data are smoothed by Friedman's 'super 
smoother' (as found in \code{\link[stats]{supsmu}}. Thereof the approximate 
1st (FD) and 2nd (FD2) derivative are calculated by a five-point stencil 
\code{link{inder}}. 

The difference of cycles at 
the maxima of the 1st and 2nd approximate derivative and a correction factor 
are used to estimate the range before the exponential phase. This simple 
function finds the background range without modeling the function. The start 
of the background range is defined be a fixed value. Since many signals tend 
to overshot in the first cycles a default value of 3 is chosen. 
\code{\link[chipPCR]{bg.max}} tries also to estimate the end of an 
amplification reaction. This feature however is experimental.
}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\examples{
# First example: Test for the background of an amplification reaction.
par(mfrow = c(2,1))
res <- AmpSim(cyc = 1:40, Cq = 25)
background <- bg.max(res)
plot(background, main = "Estimation of the Background Range\n in Absence of Noise")
res.noise <- AmpSim(cyc = 1:40, Cq = 25, noise = TRUE)
background.noise <- bg.max(res.noise)
plot(background.noise, main = "Estimation of the Background Range\n in Presence of Noise")
par(mfrow = c(1,1))


# Second example: A simple function to test for a background range.
# Data were taken form the chipPCR C17 data set.
# Note that the not the time but the "cycle number" was
# used to calculate the background range.
data(C17)
plot(C17[, 2], C17[,  3], xlab = "Cycle", ylab = "RFU", 
     main = "Estimate the begin of the Amplification\n of a HDA", pch = 20)
res <- bg.max(C17[, 2:3], bg.corr = 1.4, bg.start = 1)
abline(v = c(slot(res, "bg.start"), slot(res, "bg.stop")), col = c(1,2))
abline(h = slot(res, "fluo"), col = "blue")

# Third example: Test for the background of an amplification reaction.
# Simulate amplification curves with different quantification points
# within 40 cycles.
cyc <- seq(1, 40, 1)

# Use a five parameter model to simulate amplification curves
b <- -15; c <- 0.02; d <- 1
# Define the different quantification points with a difference of
# circa 3.32 cycles
e <- seq(21, 35, 3.32)

# Plot the amplification curves and the estimated background ranges.
plot(NA, NA, xlim = c(1, 40), ylim = c(0, 1), xlab = "Cycles", 
     ylab = "Fluorescence")
for (i in 1:length(e)) {
  fluo <- c + (d - c)/(1 + exp(b * (log(cyc) - log(e[i]))))
  points(cyc, fluo, type = "b", col = i, pch = 20)
  res <- bg.max(cyc, fluo, bg.corr = 1.4, bg.start = 1)
  abline(v = slot(res, "bg.stop"), col = i)
  abline(h = slot(res, "fluo"), col = i)
}
}
\keyword{ background }
\keyword{ range }
\keyword{ methods }
