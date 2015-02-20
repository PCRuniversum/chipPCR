\name{amptester}
\alias{amptester}

\title{
Amplification test
}
\description{
The \code{\link[chipPCR]{amptester}} function tests if an amplification is significant.
}
\usage{
amptester(y, manual = FALSE, noiselevel = 0.08, background = NULL)
}

\arguments{
  \item{y}{
        is a vector containing the fluorescence values.
}
  \item{manual}{
	switches between a statistical test (based on a Wilcoxon rank sum 
test (\code{\link[stats]{wilcox.test}})) or manual test for a positive 
amplification signal.
}
  \item{noiselevel}{
	can be set to a user defined value as threshold for a significant 
	amplification signal.
}
  \item{background}{
	is the range of the background signal in the amplification curve. 
The values can be added by the user or taken from the 
\code{\link[chipPCR]{bg.max}} function. Ignored if \code{manual} is
\code{TRUE}.
}
}
\details{
This function tries to estimate if data indicates an amplification process. 
Several instances of tests are included. The first involves a 
semiautomatic test if the range of the background is lower than the range 
of the assumed signal. To differ between the ranges an instance of 
\code{\link[chipPCR]{bg.max}} is used. Herein, this function assumes that an 
amplification takes place in case the signal of the amplification is larger 
than the \code{\link[stats]{median}} + 5 * \code{\link[stats]{mad}} than 
the background. The automatic test uses a Wilcoxon rank sum test 
\code{\link[stats]{wilcox.test}} to compare the first and the last elements 
of the data. The input values are delivered by \code{\link[utils]{head}} 
and \code{\link[utils]{tail}}, respectively. For other methods please refer 
to the references listed below. Instead of assigning a zero to negative 
amplification reaction uses the current implementation of 
\code{\link[chipPCR]{amptester}} very small random values. This is because 
some post function might fail in case all values are set to zero.
}

\value{
An object of \code{\linkS4class{amptest}} class containing result of the test as well as
the original data.
}

\references{

Frank, D. N. BARCRAWL and BARTAB: software tools for the design and 
implementation of barcoded primers for highly multiplexed DNA sequencing 
\emph{BMC bioinformatics}, 2009, Vol. 10, pp. 362

Peirson, S. N., Butler, J. N. and Foster, R. G. Experimental validation of 
novel and conventional approaches to quantitative real-time PCR data analysis 
\emph{Nucleic Acids Research}, 2003, Vol. 31(14), pp. e73-e73

Rao, X., Lai, D. and Huang, X. A New Method for Quantitative Real-Time 
Polymerase Chain Reaction Data Analysis \emph{Journal of Computational 
Biology}, 2013, Vol. 20(9), pp. 703-711

Ruijter, J. M., Ramakers, C., Hoogaars, W. M. H., Karlen, Y., Bakker, O., 
Hoff, M. J. B. v. d. and Moorman, A. F. M. Amplification efficiency: 
linking baseline and bias in the analysis of quantitative PCR data, 
\emph{Nucleic Acids Research}, 2009, Vol. 37(6), pp. e45-e45

Rutledge, R. G. and Stewart, D. A kinetic-based sigmoidal model for the 
polymerase chain reaction and its application to high-capacity absolute 
quantitative real-time PCR \emph{BMC biotechnology}, 2008, Vol. 8, pp. 47

Tichopad, A., Dilger, M., Schwarz, G. and Pfaffl, M. W. Standardized 
determination of real-time PCR efficiency from a single reaction set-up 
\emph{Nucleic Acids Research}, 2003, Vol. 31(20), pp. e122

Wilhelm, J., Pingoud, A. and Hahn, M. SoFAR: software for fully automatic 
evaluation of real-time PCR data \emph{BioTechniques}, 2003, Vol. 34(2), 
pp. 324-332

Zhao, S. and Fernald, R. D. Comprehensive Algorithm for Quantitative 
Real-Time Polymerase Chain Reaction \emph{Journal of computational biology: 
a journal of computational molecular cell biology}, 2005, Vol. 12(8), pp. 
1047-1064

}
\author{
Stefan Roediger, Michal Burdukiewicz
}

\examples{
# First example
# Arrange graphs in orthogonal matrix and set parameter for the plot.
par(las = 0, bty = "n", cex.axis = 1.5, cex.lab = 1.5, 
    font = 2, cex.main = 1.8, oma = c(1,1,1,1))
    
# Simulation of an amplification curve with 40 cycles and a Cq of
# circa 28. The amplification curve of "pos" (positive) has low 
# noise and the amplification curve of "neg" (negative) has high 
# noise.

pos <- AmpSim(cyc = 1:40, Cq = 28, noise = TRUE, nnl = 0.03)
neg <- AmpSim(cyc = 1:40, Cq = 28, noise = TRUE, nnl = 0.8)

# Plot the raw data of the simulations.

par(fig = c(0,0.5,0.5,1))
plot(NA, NA, xlim = c(1, 40), ylim = c(0, 2.1), xlab = "Cycles", 
     ylab = "Fluorescence", main = "qPCR - Raw data", type = "b")
mtext("A", cex = 2, side = 3, adj = 0, font = 2)
points(pos, col = 1, typ = "b", pch = 19)
points(neg, col = 2, typ = "b", pch = 20)
legend(1, 2, c("Positive", "Negative Control (noise)"), 
	       pch = c(19,20), col = c(1,2), lwd = 2, bty = "n")

# Plot data again after an analysis by ampteser. "neg" is set to small 
# random numbers, while "pos" remains unchanged.

par(fig = c(0,0.5,0,0.5), new = TRUE)
plot(NA, NA, xlim = c(1, 40), ylim = c(0, 2.1), xlab = "Cycles", 
     ylab = "Fluorescence", main = "qPCR - amptester", type = "b")
points(amptester(pos[, 2]), col = 1, type = "b", pch = 19)
points(amptester(neg[, 2]), col = 2, type = "b", pch = 20)
legend(1, 2, c("Positive", "Negative Control (noise)"), 
       pch = c(19,20), col = c(1,2), lwd = 2, bty = "n")

# Use of amptester for time-dependent measurements. Amplification curves 
# from the capillaryPCR data set were processed in a loop. The results of 
# amptester are added to the raw data.

par(fig = c(0.5,1,0,1), new = TRUE)
colors <- rainbow(8)
plot(NA, NA, xlim = c(0,80), ylim = c(0,1300), xlab = "Time [min]", 
     ylab = "Voltage (micro V)", main = "ccPCR")
mtext("B", cex = 2, side = 3, adj = 0, font = 2)
sapply(c(1,3,5,7), function(i) {
    xy.tmp <- cbind(capillaryPCR[1:750, i], capillaryPCR[1:750, i + 1])
    
# Use amptester to analyse the amplification curve.
# Note: The decisions of amptester can be invoked via res.ampt@decisions
# in the present example.

    res.ampt <- amptester(xy.tmp[, 2])
    
# Use the "decisions" of amptester in a logic to automatically decide if an
# amplification reaction is positive. In this example linear regression test
# (lrt.test) and the threshold test (tht.dec) are used.

    res.ampt <- ifelse(res.ampt@decisions[2] == TRUE && 
		       res.ampt@decisions[4] == TRUE, "positve", "negative")

# Plot the amplification curve with the decisions.
    lines(xy.tmp[, 1], xy.tmp[, 2], type = "b", pch = 20, col = colors[i])
    text(75, max(na.omit(xy.tmp[, 2])), res.ampt, cex = 1.3, col = colors[i])
  }
)
# Second Example
# Example to test an amplification reaction.
# Simulate first a positive amplification curve with 45 cycles and than a 
# negative amplification curve with 45 cycles. The negative amplification
# curve is created from a normal distribution
# 
fluo.neg <- rnorm(45)
fluo.pos <- AmpSim(cyc = 1:45, Cq = 45, ampl = 40, noise = TRUE, 
		   nnl = 0.03)[, 2]

plot(NA, NA, xlim = c(1, 45), ylim = c(-1, 45), xlab = "Cycles", 
     ylab = "Fluorescence", 
     main = "Simulation of a qPCR with 45 Cycles", type = "b")
points(amptester(fluo.pos), type = "b", pch = 20)
points(amptester(fluo.neg), type = "b", col = "red", pch = 20)
points(1:45, fluo.neg, col = "red")

legend(1,40, c("Positive", "Negative Control (noise)", 
       "noise pattern"), pch = c(20,20,1), col = c(1,2,2), lwd = 2)
}

\keyword{ amplification }
\keyword{ threshold }
