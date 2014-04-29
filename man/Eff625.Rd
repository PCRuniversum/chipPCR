\name{Eff625}
\alias{Eff625}
\docType{data}
\title{
Highly Replicate Number Amplification Curves
}
\description{
Highly replicate number amplification curves with an approximate amplification 
efficiency of 62.5 percent at cycle number 18. The data were derivd from a 
simulation such as the AmpSim function.
}
\usage{data(Eff625)}
\format{
  A data frame with 40 (Cycles) observations on the following 1000 
  (amplification curves) variables. The columns are all replicates.
}

\examples{
data(Eff625)

plot(NA, NA, xlim = c(1,40), ylim = c(0,max(Eff625)), xlab = "Cycle",
    ylab = "RFU", 
    main = "Amplification Curves with 62.5 Percent Efficiency")
apply(Eff625[, 1:ncol(Eff625)], 2, function(x) lines(1:40,x))

Fmean <- rowMeans(Eff625[, 1:ncol(Eff625)])
lines(1:40, Fmean, col = "red", lwd = 3)

legend(1, quantile(unlist(Eff625), 0.9), c("Raw", "Mean"), 
      pch = c(19,19), col = c(1,2))
}
\keyword{datasets}
