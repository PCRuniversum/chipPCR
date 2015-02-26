\name{C317.melt.hr}
\alias{C317.melt.hr}
\docType{data}
\title{
  Melting Curves (resolution: 0.1 degrees Celsius / step) for the Gene adk Using 
  the Bio-Rad CFX96 thermo cycler
  }
\description{
  A melting curve for adk was performed.
  }
\usage{data("C317.melt.hr")}
\format{
  A data frame with 40 observations on the following 97 variables. The first 
  column ("Temperature") contains the temperature (resolution: 0.1 degrees Celsius / step) 
  and consecutive columns 
  contain the replicates ("A1" to "H12").
  }
\details{
  adk was amplified in the Bio-Rad CFX96. After PCR, the temperature-dependent 
  change of fluorescence was simultaneously monitored (EvaGreen, Mao et al. 2007). 
  The primer sequences for adk were taken from this study.
  
  adk fw: CTCAGGCTCAGTTCATCATGGA 
  adk rv: AGTTTGCCAGCATCCATAATGTC
  
  PCR conditions: \code{\link[chipPCR]{C317.amp}}
  
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
  data(C317.melt.hr)
  str(C317.melt.hr)
  }
\keyword{datasets}
\keyword{adk}
\keyword{amplification}
\keyword{EvaGreen}