\name{C17}
\alias{C17}
\docType{data}
\title{
Helicase Dependent Amplification of HPRT1 at Different Temperatures using the 
VideoScan Platform 2.0
}
\description{
A Helicase Dependent Amplification (HDA) of HPRT1 (Homo sapiens 
hypoxanthine phosphoribosyltransferase 1) was performed at different 
temperatures in the VideoScan Platform 2.0 (similar to Roediger et al. 
(2013)). The HDA was performed at 55, 60 and 65 degrees Celsius. The optimal 
temperature for a HDA is circa 65 degrees Celsius. Lower temperatures will 
affect the slope and plateau of the HDA amplification curve.
}
\usage{data(C17)}
\format{
  A data frame with 125 observations on the following 5 variables.
  \describe{
    \item{\code{C17.t}}{Elapsed time during HDA in seconds.}
    \item{\code{C17.cycle}}{a numeric vector}
    \item{\code{C17.T55}}{Time-dependent fluorescence at 55 degrees Celsius}
    \item{\code{C17.T60}}{Time-dependent fluorescence at 60 degrees Celsius}
    \item{\code{C17.T65}}{Time-dependent fluorescence at 65 degrees Celsius}
  }
}
\details{
To perform an isothermal amplification in VideoScan 2.0, standard 
conditions for the IsoAmp(R) III Universal tHDA Kit (Biohelix) were used. 
The reaction was composed of 12.5 micro L buffer A containing 1.25 micro L 
10x reaction buffer, 150 nM primer (forward and reverse), 0.75  micro L 
template (synthetic) and A. bidest which was covered with 50 micro L 
mineral oil. The primer sequences for HPRT1 were taken from Roediger et al. 
(2013). Preincubation: This mixture was incubated for 2 min at 95 degree. 
Celsius and immediately placed on ice. 12.5 micro L of reaction buffer B 
which was composed of 1.25 micro L 10x buffer, 40 mM NaCl, 5 mM MgSO4, 1.75 
micro L dNTPs, 0.2 x EvaGreen, 1 micro L Enzyme mix and A. bidest. The 
fluorescence measurement in VideoScan HCU started directly after adding 
buffer B at 55, 60 or 65 degrees Celsius and revealed optimal conditions for 
the amplification when using 60 or 65 degrees Celsius.
Temperature profile (after Preincubation):
- 60 seconds at 65 degrees Celsius 
- 11 seconds at 55 degrees Celsius && Measurement
}
\source{
Claudia Deutschmann & Stefan Roediger, BTU Cottbus - Senftenberg, 
Senftenberg, Germany
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
data(C17)
plot(NA, NA, xlim = c(0,5000), ylim = c(0,1.2), xlab = "Time [sec]", 
     ylab = "Fluorescence", 
     main = "Temperature dependency of HDA amplification reactions")
  points(C17[, 1], C17[, 3], type = "b", col = 1, pch = 20)
  points(C17[, 1], C17[, 4], type = "b", col = 2, pch = 20)
  points(C17[, 1], C17[, 5], type = "b", col = 3, pch = 20)
legend(2000, 0.4, c("55 degrees Celsius", "60 degrees Celsius", "65 degrees Celsius"), 
	col = c(1,2,3), pch = rep(20,3))

}
\keyword{ datasets }
\keyword{ HDA }
\keyword{ HPRT1 }
\keyword{ VideoScan }