\name{Eff875}
\alias{Eff875}
\docType{data}
\title{
Highly Replicate Number Amplification Curves
}
\description{
Highly replicate number amplification curves with an approximate amplification 
efficiency of 87.5 percent at cycle number 18. The data were derivd from a 
simulation such as the AmpSim function.
}
\usage{data(Eff875)}
\format{
  A data frame with 40 (Cycles) observations on the following 1000 
  (amplification curves) variables. The columns are all replicates.
}

\examples{
data(Eff875)

plot(NA, NA, xlim = c(1,40), ylim = c(0,max(Eff875)), xlab = "Cycle",
    ylab = "RFU", 
    main = "Amplification Curves with 87.5 Percent Efficiency")
apply(Eff875[, 1:ncol(Eff875)], 2, function(x) lines(1:40,x))

Fmean <- rowMeans(Eff875[, 1:ncol(Eff875)])
lines(1:40, Fmean, col = "red", lwd = 3)

legend(1, quantile(unlist(Eff875), 0.9), c("Raw", "Mean"), 
	pch = c(19,19), col = c(1,2))
}
\keyword{datasets}
