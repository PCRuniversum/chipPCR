\name{bg.max}
\alias{bg.max}
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
bg.max(x, y, bg.corr = 1.3, bg.start = 3, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    \code{x} is a vector containing the time or cycle values.
}
  \item{y}{
        \code{y} is a vector containing the fluorescence values.
}
  \item{bg.corr}{
	a value which helps to tweak on the suggested background value of 
	\code{\link[chipPCR]{bg.max}}.
}
  \item{bg.start}{
	a user defined value for the start of the background range.
}
  \item{verbose}{
	sets if additional information such a the derivatives should be 
	provided.
}
}
\details{
background range herein refers to a level of fluorescence measured before any 
specific amplification is detectable. The raw data (e.g., fluorescence 
intensity) measured after each step (cycle or time point) follow a non-linear 
progress. The background is assumed to be constant for the entire measurement. 
The algorithm of \code{\link[chipPCR]{bg.max}} is based on the assumption 
that the signal difference of successive cycles in the linear ground phase is 
approximately constant. After transition in the early exponential phase the 
signal changes drastically. First data are smoothed by Friedman's 'super 
smoother' (as found in \code{\link[stats]{supsmu}}. Thereof the approximate 
1st (FD) and 2nd (FD2) derivative are calculated. The difference of cycles at 
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
res <- AmpSim(cyc = c(1:40), Cq = 25)
plot(res, xlim = c(1,40), ylim = c(-0.1,1), xlab = "Cycles", 
     ylab = "Fluorescence", 
     main = "Estimation of the Background Range\n in Absence of Noise", 
     type = "b", pch = 20)
background <- bg.max(res[, 1], res[, 2], verbose = TRUE)

points(background$d, col = "red", type = "b", pch = 20)
points(background$d1, col = "blue", type = "b", pch = 20)
abline(v = background$bg.start)
  text(background$bg.start, 0.2, "Background start", pos = 4)
abline(v = background$bg.stop, col = "blue")
  text(background$bg.stop, 0.25, "Background stop", pos = 4, col = "blue")
abline(v = background$amp.stop, col = "green")
  text(background$amp.stop, 0.3, "Plateau transition", pos = 4, col = "green")
legend(4, 1, c("Raw data", "First derivative", "Second derivative"), 
       pch = rep(20,3), col = c(1,2,4))

res <- AmpSim(cyc = c(1:40), Cq = 25, noise = TRUE)
plot(res, xlim = c(1,40), ylim = c(-0.1,1), xlab = "Cycles", 
     ylab = "Fluorescence", 
     main = "Estimation of the Background Range\n in Presence of Noise", 
     type = "b", pch = 20)
background <- bg.max(res[, 1], res[, 2], verbose = TRUE)

points(background$d, col = "red", type = "b", pch = 20)
points(background$d1, col = "blue", type = "b", pch = 20)
abline(v = background$bg.start)
text(background$bg.start, 0.2, "Background start", pos = 4)
abline(v = background$bg.stop, col = "blue")
text(background$bg.stop, 0.25, "Background stop", pos = 4, col = "blue")
abline(v = background$amp.stop, col = "green")
text(background$amp.stop, 0.3, "Plateau transition", pos = 4, col = "green")
legend(4, 1, c("Raw data", "First derivative", "Second derivative"), 
       pch = rep(20,3), col = c(1,2,4))
par(mfrow = c(1,1))
# Second example: A simple function to test for a background range.
# Data were taken form the chipPCR C17 data set.
# Note that the not the time but the "cycle number" was
# used to calculate the background range.
data(C17)
plot(C17[, 2], C17[,  3], xlab = "Cycle", ylab = "RFU", 
     main = "Estimate the begin of the Amplification\n of a HDA", pch = 20)
res <- bg.max(C17[, 2], C17[, 3], bg.corr = 1.4, bg.start = 3, verbose = FALSE)
abline(v = c(res$bg.start, res$bg.stop), col = c(1,2))
abline(h = res$fluo, col = "blue")

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
  res <- bg.max(cyc, fluo, bg.corr = 1.4, bg.start = 3, verbose = FALSE)
  abline(v = res$bg.stop, col = i)
  abline(h = res$fluo, col = i)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ background }
\keyword{ range }
