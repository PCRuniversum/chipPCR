\name{C126EG595}
\alias{C126EG595}
\docType{data}
\title{
qPCR Experiment for the Amplification of HPRT1 Using the Bio-Rad iQ5 thermo cycler
}
\description{
A Quantitive PCR (qPCR) with the DNA binding dye (EvaGreen) (Mao et al. 2007) 
was performed in the Bio-Rad iQ5 thermo cycler. The cycle-dependent increase 
of the fluorescence was quantified at the annealing step (59.5 deg Celsius).
}
\usage{data(C126EG595)}
\format{
  A data frame with 40 observations on the following 97 variables. The first 
  column ("Cycle") contains the number of cylces and consecutive columns 
  contain the replicates ("A01" to "H12").
}
\details{
HPRT1 was amplified in the Bio-Rad iQ5. The the change of 
fluorescence was simultaniously monitored for the Hydrolysis probe of HPRT1 
and EvaGreen. The primer sequences for HPRT1 were taken from Roediger et al. 
(2013). A 10 micro L qPCR reaction was composed of 250 nM primer (forward and 
reverse), qPCR Mix (accordingt to the manufactures recommendations), 1 micro L 
template  (HPRT1 amplification product), 60 nM hydorlysis probe probe for 
HPRT1. EvaGreen was used at 0.5x final. During the amplification was monitored 
59.5 degree Celsius.
}
\source{
Stefan Roediger, Clauia Deutschman (BTU Cottbus - Senftenberg)
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
data(C126EG595)
tmp  <- C126EG595

plot(NA,NA, xlim = c(1,40), ylim = c(min(tmp[, 2:ncol(tmp)]), 
   max(tmp[, 2:ncol(tmp)])), xlab = "Cycle", ylab = "RFU (FAM)", 
   main = "Amplification monitored at \n58.5 degree Celsius (annealing step)")
apply(tmp[, 2:ncol(tmp)], 2, 
      function(x) lines(tmp[1:nrow(tmp),1],x))
}
\keyword{ hydrolysis }
\keyword{ EvaGreen }
\keyword{ HPRT1 }
