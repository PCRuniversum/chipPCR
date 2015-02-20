\name{Eff1000}
\alias{Eff1000}
\docType{data}
\title{
Highly Replicate Number Amplification Curves
}
\description{
Highly replicate number amplification curves with an approximate amplification 
efficiency of 100 percent at cycle number 18. The data were derived from a 
simulation such as the AmpSim function.
}
\usage{data(Eff1000)}
\format{
  A data frame with 40 (Cycles) observations on the following 1000 
  (amplification curves) variables. The columns are all replicates.
}

\examples{
data(Eff1000)

plot(NA, NA, xlim = c(1,40), ylim = c(0,max(Eff1000)), xlab = "Cycle",
    ylab = "RFU", 
    main = "Amplification Curves with 100 Percent Efficiency")
apply(Eff1000[, 1:ncol(Eff1000)], 2, function(x) lines(1:40,x))

Fmean <- rowMeans(Eff1000[, 1:ncol(Eff1000)])
lines(1:40, Fmean, col = "red", lwd = 3)

legend(1, quantile(unlist(Eff1000), 0.9), c("Raw", "Mean"), 
	pch = c(19,19), col = c(1,2))
}
\keyword{datasets}
