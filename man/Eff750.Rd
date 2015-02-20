\name{Eff750}
\alias{Eff750}
\docType{data}
\title{
Highly Replicate Number Amplification Curves
}
\description{
Highly replicate number amplification curves with an approximate amplification 
efficiency of 75 percent at cycle number 18. The data were derived from a 
simulation such as the AmpSim function.
}
\usage{data(Eff750)}
\format{
  A data frame with 40 (Cycles) observations on the following 1000 
  (amplification curves) variables. The columns are all replicates.
}
\examples{
data(Eff750)

plot(NA, NA, xlim = c(1,40), ylim = c(0,max(Eff750)), xlab = "Cycle",
    ylab = "RFU", 
    main = "Amplification Curves with 75 Percent Efficiency")
apply(Eff750[, 1:ncol(Eff750)], 2, function(x) lines(1:40,x))

Fmean <- rowMeans(Eff750[, 1:ncol(Eff750)])
lines(1:40, Fmean, col = "red", lwd = 3)

legend(1, quantile(unlist(Eff750), 0.9), c("Raw", "Mean"), 
	pch = c(19,19), col = c(1,2))
}
\keyword{datasets}
