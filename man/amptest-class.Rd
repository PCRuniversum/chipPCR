\name{amptest}
\docType{class}
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
    \item{\code{decision}:}{\code{"integer"} states if the amplification 
			    is "positive" or "negative"}
    \item{\code{noiselevel}:}{ \code{"numeric"} user-defined threshold for a 
			      significant amplification signal.}
    \item{\code{noise}:}{ \code{"logical"} \code{TRUE} if noise was detected.}
    \item{\code{linearity}:}{ \code{"logical"} \code{TRUE} if data are 
				likely from a amplification curve.}
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
fluo.pos <- AmpSim(cyc = 1:40)[, 2]
ampt.pos <- amptester(fluo.pos, manual = TRUE, background = c(0, 15))
plot(ampt.pos, xlab = "Cycles", ylab = "RFU")
abline(v = c(0,15), col = 2)
summary(ampt.pos)

fluo.neg <- AmpSim(cyc = 1:40, noise = TRUE, nnl = 1, ampl = 0.1)[, 2]
ampt.neg <- amptester(fluo.neg, manual = TRUE, background = c(0, 15), 
		      noiselevel = 0.3)
plot(ampt.neg, xlab = "Cycles", ylab = "RFU")
abline(v = c(0,15), col = 2)
summary(ampt.neg)
}

\keyword{classes}