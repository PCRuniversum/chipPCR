\name{VIMiQ5_melt}
\alias{VIMiQ5_melt}
\docType{data}
\title{
Melting Curve Measured with the Bio-Rad iQ5
}
\description{
Data set of a melting curve using the Bio-Rad iQ5 thermo cycler. The 
samples of Vimentin were measured in the CFX96 as replicates according to 
Roediger et al. (2013). The qunatitiation was performed during the gradient 
from 55 to 95 deg Celsius with a resolution of 0.5 deg Celsius per step.
}
\usage{data(VIMiQ5_melt)}
\format{
  A data frame with 81 observations on the following 97 variables. The 
first column ("T") contains the temperature and consecutive columns contain 
the replicates ("A01" to "H12").
  }
\source{
Stefan Roediger, Clauia Deutschman (BTU Cottbus - Senftenberg)
}
\references{
A Highly Versatile Microscope Imaging Technology Platform for the Multiplex 
Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. Roediger, 
P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, M. 
Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder. 
\emph{Advances in Biochemical Bioengineering/Biotechnology}. 133:33--74, 
2013. \url{http://www.ncbi.nlm.nih.gov/pubmed/22437246}
}
\examples{
data(VIMiQ5_melt)
tmp <- VIMiQ5_melt

plot(NA, NA, xlim = c(55,95), ylim = c(0, 40000), 
    xlab = "Temperature (deg Celsius)",ylab = "RFU", 
    main = "Melting curve in iQ5 (Bio-Rad)")
apply(tmp[, 2:ncol(tmp)], 2, 
      function(x) lines(tmp[1:nrow(tmp),1],x))

Fmean <- rowMeans(tmp[, 2:ncol(tmp)])
lines(tmp[1:nrow(tmp),1], Fmean, col = "red", lwd = 3)

legend(55, 4000, c("Raw", "Mean"), pch = c(19,19), col = c(1,2))
}
\keyword{datasets}
\keyword{melt}
\keyword{iQ5}
