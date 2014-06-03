\name{amptest}
\docType{class}
\alias{amptest}
\alias{amptest-class}
\alias{summary.amptest}
\alias{summary,amptest-method}
\alias{show.amptest}
\alias{show,amptest-method}

\title{Class \code{"amptest"}}
\description{
An S4 class containing the output \code{\link[chipPCR]{amptester}} 
function.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"numeric"} is a vector containing the 
			 fluorescence values.}
    \item{\code{decisions}:}{\code{"logical"} contains outcomes of various tests.
    \code{shap.noisy} is presence of noise, \code{lrt.test} states if data are 
  			likely from a amplification curve and both \code{tht.dec} and \code{tht.dec}
        defines if the amplification is "positive" or "negative".}
    \item{\code{noiselevel}:}{ \code{"numeric"} user-defined threshold for a 
			      significant amplification signal.}
    \item{\code{background}:}{range of the background signal in the 
			      amplification curve.}
}
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "amptest")}: prints summary 
		   of the object.}
    \item{show}{\code{signature(object = "amptest")}: prints only 
		\code{.Data} slot of the object.}
   }
}
\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link[chipPCR]{amptester}}
}
\examples{
# Compare a positive and a negative amplification reaction.
# First simulate positive reaction (fluo.pos) and than the 
# negative reaction (fluo.neg).
# Simulation of an amplifiaction curve with some noise and a high signal.

fluo.pos <- AmpSim(cyc = 1:40, noise = TRUE)[, 2]
ampt.pos <- amptester(fluo.pos, manual = TRUE, background = c(0, 15), 
		      noiselevel = 0.15)

# Plot amplification curve and result of amptester
plot(fluo.pos, xlab = "Cycles", ylab = "RFU", pch = 19, ylim = c(0, 1))
lines(ampt.pos, col = 2, pch = 19, type = "b")
legend(5, 1, c("Raw data", "amptester output"), 
       col = c(1,2,3), bty = "n", pch = c(19, 19))
# Borders for background calculation
abline(v = c(0,15), col = 2)
# Level for background threshold
abline(h = 0.15, col = 3, lty = 2)
text(5, 0.18, "Noise threshold")
# Summary of amptester results 
summary(ampt.pos)

# Simulation of an amplifiaction curve with high noise and a low signal.

fluo.neg <- AmpSim(cyc = 1:40, noise = TRUE, ampl = 0.13, nnl = 0.4)[, 2]
ampt.neg <- amptester(fluo.neg, manual = TRUE, background = c(0, 15), 
		      noiselevel = 0.15)

# Plot amplification curve and result of amptester
plot(fluo.neg, xlab = "Cycles", ylab = "RFU", pch = 19, ylim = c(0, 1))
lines(ampt.neg, col = 2, pch = 19, type = "b")
legend(5, 1, c("Raw data", "amptester output"), 
       col = c(1,2,3), bty = "n", pch = c(19, 19))
# Borders for background calculation
abline(v = c(0,15), col = 2)
# Level for background threshold
abline(h = 0.15, col = 3, lty = 2)
text(5, 0.18, "Noise threshold")
# Summary of amptester results
summary(ampt.neg)
}

\keyword{classes}