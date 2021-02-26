\name{C317.amp}
\alias{C317.amp}
\docType{data}
\title{
  qPCR Experiment for the Amplification of adk Using the Bio-Rad CFX96 thermo cycler
  }
\description{
  A quantitative real-time PCR of adk was performed.
  }
\usage{data("C317.amp")}
\format{
  A data frame with 40 observations on the following 97 variables. The first 
  column ("Cycle") contains the number of cycles and consecutive columns 
  contain the replicates ("A01" to "H12").
  }
\details{
  adk was amplified in the Bio-Rad CFX96. The change of fluorescence was 
  simultaneously monitored (EvaGreen, Mao et al. 2007). The 
  primer sequences for adk were taken from this study.
  
  gDNA: 28.43 ng/microL DNA concentration, 260/280 ratio= 1.96
  
  adk fw: CTCAGGCTCAGTTCATCATGGA 
  adk rv: AGTTTGCCAGCATCCATAATGTC
  
  PCR conditions:
    10 minutes at 95 degrees Celsius
    40 x {
      30 seconds at 95 degrees Celsius
      45 seconds at 59 degrees Celsius
      45 seconds at 68 degrees Celsius
      }
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
  
  
  Mao, F., Leung, W.-Y., Xin, X., 2007. Characterization of EvaGreen and the 
  implication of its physicochemical properties for qPCR applications. 
  \emph{BMC Biotechnol}. 7, 76.
  }
\examples{
  data(C317.amp)
  str(C317.amp)
  }
\keyword{datasets}
\keyword{adk}
\keyword{amplification}
\keyword{EvaGreen}
