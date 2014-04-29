\name{C81}
\alias{C81}
\docType{data}
\title{
Helicase Dependent Amplification of pCNG1 using the VideoScan Platform
}
\description{
A Helicase Dependent Amplification (HDA) of pCNG1 was performed. The VideoScan 
Platform (Roediger et al. (2013)) was used to monitor the amplification. The 
HDA was performed at 65 degree Celsius. Two concentrations of input DNA were 
used.
}
\usage{data(C81)}
\format{
  A data frame with 351 observations on the following 5 variables.
  \describe{
    \item{\code{Cycle}}{Cycles HDA measurements.}
    \item{\code{t.D1}}{Dilution 1, elapsed time during HDA in seconds.}
    \item{\code{MFI.D1}}{Dilution 1, fluorescence.}
    \item{\code{t.D2}}{Dilution 2, elapsed time during HDA in seconds.}
    \item{\code{MFI.D2}}{Dilution 2, fluorescence.}
  }
}
\details{

To perform an isothermal amplification in VideoScan, standard conditions for 
the IsoAmp(R) III Universal tHDA Kit (Biohelix) were used. The reaction was 
composed of reaction mix A)10 micro L A. bidest, 1.25 micro L 10xbuffer, 
0.75 micro L primer(150nM final), 0.5 micro L template plasmid. Preincubation: 
This mixture was incubated for 2 min at 95 degree. Celsius and immediately 
placed on ice. Reaction mix B) 5 micro L A. bidest., 1.25 micro L 10x buffer, 
2 micro L NaCl, 1.25 micro L MgSO4, 1,75 micro L dNTPs, 0.25 micro L EvaGreen, 
1 micro L enzyme mix. The mix was covered with 50 micro L mineral oil. The 
fluorescence measurement in VideoScan HCU started directly after adding 
buffer B at 65 degree Celsius. A 1x (D1) and a 1:10 dilution (D2) were tested.
Temperature profile (after Preincubation):
- 60 seconds at 65 degree Celsius 
- 11 seconds at 55 degree Celsius && Measurement
}
\source{
Claudia Deutschmann & Stefan Roediger, BTU Cottbus - Senftenberg, Senftenberg, 
Germany
}
\references{
A Highly Versatile Microscope Imaging Technology Platform for the Multiplex 
Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. Roediger, 
P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, 
M. Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder.
\emph{Advances in Biochemical Bioengineering/Biotechnology}. 133:33--74, 2013.
\url{http://www.ncbi.nlm.nih.gov/pubmed/22437246}
}
\examples{
data(C81)
# First example
# Comparison of Lowess, Moving average and splines to smooth amplification curve 
# data of
# HDA for pCNG1.

plot(NA, NA, xlim = c(0, 120), ylim = c(0, 1.2), xlab = "Time [min]", 
     ylab = "Fluorescence", main = "VideScan HCU HDA amplification - Raw data")
  points(C81[, 2]/60, C81[, 3], type = "b", col = 1, pch = 20)
  points(C81[, 4]/60, C81[, 5], type = "b", col = 2, pch = 20)
legend(2000, 0.4, c("D1", "D2"), col = c(1,2), pch = rep(20,2))

}
\keyword{ datasets }
\keyword{ HDA }
\keyword{ EvaGreen }
\keyword{ HCU }
\keyword{ VideoScan }