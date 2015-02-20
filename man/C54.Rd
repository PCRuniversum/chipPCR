\name{C54}
\alias{C54}
\docType{data}
\title{
qPCR Experiment for the amplification of MLC-2v using the VideoScan 
heating/cooling-unit
}
\description{
qPCR Experiment for the amplification of MLC-2v using the VideoScan 
heating/cooling-unit
}
\usage{data(C54)}
\format{
  A data frame with 56 observations on the following 4 variables.
  \describe{
    \item{\code{Cycle}}{Cycle number}
    \item{\code{D1}}{Stock concentration of input cDNA}
    \item{\code{D2}}{1/10 diluted stock cDNA}
    \item{\code{D3}}{1/100 diluted stock cDNA}
  }
}
\details{
The aim was to amplify MLC-2v in the VideoScan and to monitor with a 
hydrolysis probe for MLC-2v. The primer sequences for MLC-2v were taken 
from Roediger et al. (2013). The amplification was detected in solution of 
the 1 HCU (see Roediger et al. 2013 for details). A 20 micro L PCR reaction 
was composed of 250 nM primer (forward and reverse), 1x Maxima Probe qPCR 
Master Mix (Fermentas), 1 micro L template (MLC-2v amplification product in 
different dilutions), 50 nM hydrolysis probe probe for MLC-2v and A. 
bidest. During the amplification, fluorescence was measured at 59.5 degree 
Celsius. The Cy5 channel was used to monitor the MLC-2v specific hydrolysis 
probe. Input stock cDNA was used undiluted (D1). D2 was 1/1000 and D3 
1/1000000 diluted in A. bidest. The D1, D2, and D3 have different numbers 
measure points and D2 contains a missing value at cycle 37.

}
\source{
Stefan Roediger, Claudia Deutschmann
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
data(C54)
str(C54)
plot(NA, NA, xlim = c(0,56), ylim = c(0, 0.7), xlab = "Cycle", 
     ylab = "refMFI")
apply(C54[, c(2:4)], 2, function(x) lines(C54[, 1], x))
}
\keyword{VideoScan}
\keyword{MLC-2v}