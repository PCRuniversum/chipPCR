\name{C67}
\alias{C67}
\docType{data}
\title{
Helicase Dependent Amplification of HPRT1 with different input DNA 
quantities using the Bio-Rad iQ5 thermo cycler
}
\description{
A Helicase Dependent Amplification (HDA) of HPRT1 (Homo sapiens 
hypoxanthine phosphoribosyltransferase 1) was performed at three different 
input DNA quantities using the Bio-Rad iQ5 thermo cycler. The HDA was 
performed at 65 degree Celsius. The optimal temperature for a HDA is circa 
65 degree Celsius. Lower temperatures will affect the slope and plateau of 
the HDA amplification curve.
}
\usage{data(C67)}
\format{
  A data frame with 43 observations on the following 6 variables.
  \describe{
    \item{\code{Cycles.C67}}{a numeric vector containing the cycle numbers}
    \item{\code{t.C67}}{a numeric vector containing the time elapsed 
between the cycles. The time was calculated by the cycle duration of one
iQ5 thermocycler step (71 seconds / step).}
    \item{\code{D1}}{Dilution 1.}
    \item{\code{D2}}{Dilution 2.}
    \item{\code{D3}}{Dilution 3.}
    \item{\code{D4}}{Dilution 4.}
  }
}
\details{
To perform an isothermal amplification in VideoScan, standard conditions 
for the IsoAmp(R) III Universal tHDA Kit (Biohelix) were used. The reaction 
was composed of 12.5 micro L buffer A containing 1.25 micro L 10x reaction 
buffer, 150 nM primer (forward and reverse), 0.75  micro L template 
(synthetic) and A. bidest which was covered with 50 micro L mineral oil. 
The primer sequences for HPRT1 were taken from Roediger et al. (2013). 
Preincubation: This mixture was incubated for 2 min at 95 degree. Celsius 
and immediately placed on ice. 12.5 micro L of reaction buffer B which was 
composed of 1.25 micro L 10x buffer, 40 mM NaCl, 5 mM MgSO4, 1.75 micro L 
dNTPs, 0.2 x Evagreen, 1 micro L Enzyme mix and A. bidest. The fluorescence 
measurement started directly after adding buffer B and the preincubation step.
Temperature profile if the iQ5 thermo cycler (after Preincubation):
- 60 seconds at 65 degree Celsius 
- 11 seconds at 55 degree Celsius && Measurement
}
\source{
Claudia Deutschmann & Stefan Roediger, BTU Cottbus - Senftenberg, 
Senftenberg, 
Germany
}
\references{
A Highly Versatile Microscope Imaging Technology Platform for the 
Multiplex 
Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. 
Roediger, 
P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, 
M. Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder.
\emph{Advances in Biochemical Bioengineering/Biotechnology}. 133:33--74, 
2013.
\url{http://www.ncbi.nlm.nih.gov/pubmed/22437246}
}
\examples{
data(C67)
## maybe str(C67) ; plot(C67) ...
}
\keyword{datasets}
