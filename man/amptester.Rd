\name{amptester}
\alias{amptester}

\title{
Amplification test
}
\description{
The function \code{\link[chipPCR]{amptester}} can be used to test if an 
amplification is significant.
}
\usage{
amptester(y, manual = FALSE, noiselevel = 0.08, background = NULL)
}

\arguments{
  \item{y}{
        is a vector containing the fluorescence values.
}
  \item{manual}{
	switches between a statistical test (based on a t-test 
	(\code{\link[stats]{t.test}})) or 
	manual test for a positive amplification signal.
}
  \item{noiselevel}{
	can be set to a user defined value as threshold for a significant 
	amplification signal.
}
  \item{background}{
	is the range of the background signal in the amplification curve. The 
	values can be added by the user or taken from the 
	\code{\link[chipPCR]{bg.max}} function.
}
}
\details{
  The function tries to estimate if a amplification process is taking 
place. Several instances of tests are included. The first 
involves a semiautomatic test if the range of the background is lower than 
the range of the assumed signal. To differ between the ranges an instance 
of \code{\link[chipPCR]{bg.max}} is used. Herein, the function assumes that 
an amplification takes place in case the signal of the amplification is 
larger than the \code{\link[stats]{median}} + 5 * \code{\link[stats]{mad}} 
than the background. The automatic test uses a t-test 
\code{\link[stats]{t.test}} to compare the first and the last elements of the 
data. The input values are delivered by \code{\link[utils]{head}} and 
\code{\link[utils]{tail}}, respectively. For other methods please refer to 
the references listed below. Instead of assigning a zero to negative 
amplification reaction uses the current implementation of 
\code{\link[chipPCR]{amptester}} very small random values. This is because 
some post function might fail in case all values are set to zero.
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

# Plot the raw data of the simulations
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
for (i in c(1,3,5,7)) {
  xy.tmp <- cbind(capillaryPCR[1:750, i], capillaryPCR[1:750, i+1])
  lines(xy.tmp[, 1], xy.tmp[, 2], type = "b", pch = 20, col = colors[i])
  text(75, max(na.omit(xy.tmp[, 2])), amptester(xy.tmp[, 2])@decision, 
	cex = 1.3, col = colors[i])
}

# Second Example
# Example to test an amplification reaction.
# Simulate first a positive amplification curve with 35 cycles and than a 
# negative amplification curve with 35 cycles. The negative amplification
# curve is created from a normal distribution
# 
cycno <- 35 # Number of cycles

cyc <- seq(1, cycno <- 35, 1)
# Coefficients of a 5 parameter sigmoid model. 
b <- -14.29; c <- 0.18; d <- 39.18; e <- 19.14
fluo.pos <- c + (d - c)/(1 + exp(b * (log(cyc) - log(e))))
fluo.neg <- rnorm(cycno <- 35)

plot(NA, NA, xlim = c(1, cycno <- 35), ylim = c(-1, 40), xlab = "Cycles", 
     ylab = "Fluorescence", 
     main = "Simulation of a qPCR with 35 Cycles", type = "b")
points(amptester(fluo.pos), type = "b", pch = 20)
points(amptester(fluo.neg), type = "b", col = "red", pch = 20)
points(cyc, fluo.neg, col = "red")

legend(1,40, c("Positive", "Negative Control (noise)", 
	"noise pattern"), pch = c(20,20,1), col = c(1,2,2), 
	lwd = 2)
}

\keyword{ amplification }
\keyword{ threshold }
