\name{C316.melt.lr}
\alias{C316.melt.lr}
\docType{data}
\title{
  Melting Curves (resolution: 0.5 degrees Celsius / step) for the Gene adk Using the Bio-Rad iQ5 thermo cycler
}
\description{
  A melting curve for adk was performed.
}
\usage{data("C316.melt.lr")}
\format{
  A data frame with 40 observations on the following 97 variables. The first 
  column ("T") contains the temperature (resolution: 0.5 degrees Celsius / step) and consecutive columns 
  contain the replicates ("A01" to "H12").
}
\details{
  adk was amplified in the Bio-Rad iQ5. After PCR, the temperature-dependent 
  change of fluorescence was simultaneously monitored (EvaGreen, Mao et al. 2007). 
  The primer sequences for adk were taken from this study.

  adk fw: CTCAGGCTCAGTTCATCATGGA 
  adk rv: AGTTTGCCAGCATCCATAATGTC
  
  PCR conditions: \code{\link[chipPCR]{C316.amp}}
}
\source{
Stefan Roediger, Claudia Deutschmann, Claudia Zelck (BTU Cottbus - Senftenberg)
}
\references{
  A Highly Versatile Microscope Imaging Technology Platform for the Multiplex 
  Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. Roediger, 
  P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, 
  M. Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder.
  \emph{Advances in Biochemical Bioengineering/Biotechnology}. 133:33--74, 2013.
  \url{http://www.ncbi.nlm.nih.gov/pubmed/22437246}
  
  Mao, F., Leung, W.-Y., Xin, X., 2007. Characterization of EvaGreen and the 
  implication of its physicochemical properties for qPCR applications. 
  \emph{BMC Biotechnol}. 7, 76.
}
\examples{
data(C316.melt.lr)
str(C316.melt.lr)
}
\keyword{datasets}
\keyword{adk}
\keyword{amplification}
\keyword{EvaGreen}