\name{bg.max}
\alias{bg.max}
\alias{bg.max.numeric}
\alias{bg.max.matrix}
\alias{bg.max.data.frame}
\alias{bg.max,numeric,numeric-method}
\alias{bg.max,matrix,missing-method}
\alias{bg.max,data.frame,missing-method}

\title{
Simple function to detect and correct the background range
}
\description{
The function \code{\link[chipPCR]{bg.max}} is intended to detect and 
correct background noise. The detection is made without any assumptions 
regarding the model of the function.
}
\usage{
\S4method{bg.max}{numeric,numeric}(x, y, bg.corr = 1.3, bg.start = 2, 
		  inder.approx = TRUE)

\S4method{bg.max}{matrix,missing}(x, y, bg.corr = 1.3, bg.start = 2, 
		  inder.approx = TRUE)

\S4method{bg.max}{data.frame,missing}(x, y, bg.corr = 1.3, bg.start = 2, 
		  inder.approx = TRUE)
}

\arguments{
  \item{x}{
    is a vector containing the time or cycle values or data frame/matrix 
containing cycle in the first column and fluorescence values in the second 
column. 
}
  \item{y}{
    is a vector containing the fluorescence values. Used only if \code{x} 
is also a vector. 
}
  \item{bg.corr}{
  	a value which helps to tweak on the suggested background value of 
    \code{\link[chipPCR]{bg.max}}.
}
  \item{bg.start}{
	  a user defined value for the start of the background range.
}
  \item{inder.approx}{
    a \code{logical} value defining if data should be numerically derived 
by \code{\link[chipPCR]{inder}} function. If \code{FALSE}, derivatives are calculated by the
\code{\link[stats]{predict.smooth.spline}}.
}
}
\details{
Background range herein refers to a level of fluorescence measured before 
any specific amplification is detectable. The raw data (e.g., fluorescence 
intensity) measured after each step (cycle or time point) follow a 
non-linear progress. The background is assumed to be constant for the 
entire measurement. The algorithm of \code{\link[chipPCR]{bg.max}} is based 
on the assumption that the signal difference of successive cycles in the 
linear ground phase is approximately constant. After transition in the 
early exponential phase the signal changes drastically. First data are 
smoothed by Friedman's 'super smoother' (as found in 
\code{\link[stats]{supsmu}}. Thereof the approximate first and second 
derivative are calculated by a five-point stencil 
\code{link[chipPCR]{inder}}. 

The difference of cycles at the maxima of the first and second approximate 
derivative and a correction factor are used to estimate the range before 
the exponential phase. This simple function finds the background range 
without modeling the function. the start of the background range is defined 
be a fixed value. since many signals tend to overshot in the first cycles a 
default value of 3 is chosen. \code{\link[chipPCR]{bg.max}} tries also to 
estimate the end of an amplification reaction.

--
The following paragraphs describe methods to detect the background range 
of amplification curves. Currently none of them is implemented as R 
function.

The easiest way to classify them is the extend of assumptions made before 
applying of a method. The simplest approach is to treat the background 
fluorescence as a value constant during whole amplification reaction. In 
this case the noise could be approximated as the mean or median of 
fluorescence values in lag phase (Frank (2009)) or their standard 
deviations (Peirson et al. (2003)). The more sophisticated way of 
approximating constant background fluorescence requires optimizing its 
value to achieve linearity of the model fit on the semi logarithmic plot 
in log-linear phase (Frank (2009)). The latter procedure is greatly 
enhanced by performing further computations only on a subset of 
consecutive measurements for which calculated efficiencies have the lowest 
variance. Other methods loosen the assumption that background fluorescence 
is a constant value and instead describe it as a function of the cycle 
number. For example the algorithm used in SoFar (Wilhelm et al. (2003)) 
fits a nonlinear saturation function to measurement points before the start 
of the exponential growth phase. Parameters of the saturation function are 
chosen to minimize the sum of squared residuals of the fitted function. 
Then the value of saturation function is calculated for all data points 
and subtracted from measured values giving corrected values of 
fluorescence, which are used in next calculations.

Some approaches make even less assumptions regarding the form of the 
background noise. The taking-difference linear regression method has a 
premise that changes of fluorescence between subsequent cycles are 
exclusively caused by the amplification of the product (Rao et al. 
(2013)). 
The corrected values are calculated by simply subtracting the fluorescence 
value in the former cycle from fluorescence in the latter. Of course in 
this case the real fluorescence value in first cycle is unknown, so the 
number of cycles that can be used in following computations is reduced by 
one.

The Real-Time PCR Miner algorithm is also nearly assumption-free 
(Zhao (2005)). The main principle is that background fluorescence is 
similar in the small groups of subsequent measurements. So the first step 
of the algorithm is division of subsequent measurement points belonging to 
the exponential phase of amplification in at least four-element groups. 
For each set of points is calculated a pair of the estimate of the 
efficiency and the significance of model representing relation between the 
fluorescence value and the cycle number. The estimates paired with the 
highest significance are the most influential in the computation of the 
final efficiency.

To find the beginning of the lag phase and end of plateau phase is 
important for the goodness-of-fit for both exponential-phase-only and 
S-shaped models. There are two strategies. The first narrows the area of 
the search to the neighborhood of their theoretical values determined by a 
fitted model of the amplification reaction. To this group belongs 
SoFar (Wilhelm et al. (2003)). The algorithm looks for the start and 
the end of the exponential phase near the second derivatives of the 
function representing the relation between logarithm of the fluorescence 
and the cycle number. The available correction guarantees that the start 
of amplification has higher value than background noise. The very similar 
procedure is implemented in Real-Time PCR Miner (Zhao (2005)), 
where background noise is also used as parameter in implemented models to 
calculate theoretical the start of the amplification process. The end of 
amplification process is detected by calculating the third derivative of 
implemented S-shaped model. The second approach does not require 
theoretical values. A very intuitive solution, designated take-off point, 
by Tichopad et al. (2003) describes the lag phase using a linear function. 
Random deviations are taken into account as standardized residuals. The 
method starts with a fitting of a linear function to first three 
measurement points. If none of residuals is considered an outlier with a 
statistical test, the algorithm fits a new linear model to the first four 
measurement points and so on. The procedure stops when two last points are 
designated as outliers. The first of aforementioned outliers is considered 
the end of lag phase. It is worth noting that this algorithm is versatile 
enough to also detect the beginning of the plateau phase.
}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\references{
D. N. Frank. BARCRAWL and BARTAB: software tools for the design and 
implementation of barcoded primers for highly multiplexed DNA sequencing. 
\emph{BMC Bioinformatics}, 10:362, 2009. ISSN 1471-2105. doi: 
10.1186/1471-2105-10-362. PMID: 19874596 PMCID: PMC2777893.

S. N. Peirson, J. N. Butler, and R. G. Foster. Experimental validation of 
novel and conventional approaches to quantitative real-time PCR data 
analysis. \emph{Nucleic Acids Research}, 31(14):e73, July 2003. ISSN 
1362-4962. PMID: 12853650 PMCID: PMC167648.

X. Rao, D. Lai, and X. Huang. A new method for quantitative real-time 
polymerase chain reaction data analysis. \emph{Journal of computational 
biology: a journal of computational molecular cell biology}, 
20(9):703--711, Sept. 2013. ISSN 1557-8666. doi: 10.1089/cmb.2012.0279. 
PMID: 23841653 PMCID: PMC3762066.

A. Tichopad, M. Dilger, G. Schwarz, and M. W. Pfaffl. Standardized 
determination of real-time PCR efficiency from a single reaction set-up. 
\emph{Nucleic Acids Research}, 31(20):e122, Oct. 2003. ISSN 1362-4962. 
PMID: 14530455 PMCID: PMC219490.

J. Wilhelm, A. Pingoud, and M. Hahn. Real-time PCR-based method for the 
estimation of genome sizes. \emph{Nucleic Acids Research}, 31(10):e56, May 
2003. ISSN 0305-1048. \url{http://www.ncbi.nlm. 
nih.gov/pmc/articles/PMC156059/}. PMID: 12736322 PMCID: PMC156059.

S. Zhao and R. D. Fernald. Comprehensive algorithm for quantitative 
real-time polymerase chain reaction. \emph{Journal of computational 
biology: a journal of computational molecular cell biology}, 12(8): 
1047--1064, Oct. 2005. ISSN 1066-5277. doi: 10.1089/cmb.2005.12.1047. PMID: 
16241897 PMCID: PMC2716216.

}

\examples{
# First example: Test for the background of an amplification reaction.
par(mfrow = c(2,1))
res <- AmpSim(cyc = 1:40, Cq = 25)
background <- bg.max(res)
plot(background, main = "Estimation of the Background Range\n
in Absence of Noise")
res.noise <- AmpSim(cyc = 1:40, Cq = 25, noise = TRUE)
background.noise <- bg.max(res.noise)
plot(background.noise, main = "Estimation of the Background Range\n
in Presence of Noise")
par(mfrow = c(1,1))


# Second example: A simple function to test for a background range.
# Data were taken form the chipPCR C17 data set.
# Note that the not the time but the "cycle number" was
# used to calculate the background range.
data(C17)
plot(C17[, 2], C17[,  3], xlab = "Cycle", ylab = "RFU", 
     main = "Estimate the begin of the Amplification\n of a HDA", 
     pch = 20)
res <- bg.max(C17[, 2:3], bg.corr = 1.4, bg.start = 1)
abline(v = c(slot(res, "bg.start"), slot(res, "bg.stop")), 
       col = c(1,2))
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
