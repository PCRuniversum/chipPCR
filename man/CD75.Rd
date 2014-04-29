\name{CD75}
\alias{CD75}
\docType{data}
\title{
Helicase Dependent Amplification in the VideoScan HCU
}
\description{
Helicase Dependent Amplification in the VideoScan HCU of HRPT1 (Homo sapiens h
ypoxanthine phosphoribosyltransferase 1)
}
\usage{data(CD75)}
\format{
  A data frame with 93 observations on the following 6 variables. The data 
  frame contains three replicates of a HDA for HPRT1.
  \describe{
    \item{\code{CD75.t1}}{Elapsed time during HDA in seconds.}
    \item{\code{CD75.F1}}{Time-dependent fluorescence during HDA.}
    \item{\code{CD75.t2}}{Elapsed time during HDA in seconds.a numeric vector}
    \item{\code{CD75.F2}}{Time-dependent fluorescence during HDA}
    \item{\code{CD75.t3}}{Elapsed time during HDA in seconds.}
    \item{\code{CD75.F3}}{Time-dependent fluorescence during HDA.}
  }
}
\details{
To perform an isothermal amplification in VideoScan, standard conditions for 
the IsoAmp(R) III Universal tHDA Kit (Biohelix) were used. The reaction was 
composed of 12.5 micro L buffer A containing 1.25 micro L 10x reaction buffer, 
150 nM primer (forward and reverse), 0.75  micro L template (synthetic) and 
A. bidest which was covered with 50 micro L mineral oil. The primer sequences 
for HPRT1 were taken from Roediger et al. (2013). Preincubation: This mixture 
was incubated for 2 min at 95 degree. Celsius and immediately placed on ice. 
12.5 micro L of reaction buffer B which was composed of 1.25 micro L 10x 
buffer, 40 mM NaCl, 5 mM MgSO4, 1.75 micro L dNTPs, 0.2 x Evagreen, 1 micro L 
Enzyme mix and A. bidest. The fluorescence measurement in VideoScan HCU 
started directly after adding buffer B at 55, 60 or 65 degree Celsius and 
revealed optimal conditions for the amplification when using 60 or 65 degree 
Celsius. Temperature profile (after Preincubation):
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
data(CD75)
## maybe str(CD75) ; plot(CD75) ...
}
\keyword{datasets}
