\name{C126EG685}
\alias{C126EG685}
\docType{data}
\title{
qPCR Experiment for the Amplification of HPRT1 Using the Roche Light Cycler 
1.5
}
\description{
A quantitative PCR (qPCR) with the DNA binding dye (EvaGreen) (Mao et al. 
2007) was performed in the Roche Light Cycler 1.5 thermo cycler. The 
cycle-dependent increase of the fluorescence was quantified at the 
elongation step (68.5 degrees Celsius).
}
\usage{data(C126EG685)}	
\format{
  A data frame with 40 observations on the following 97 variables. The 
first 
  column ("Cycles") contains the number of cycles and consecutive columns 
  contain the replicates ("A01" to "H12").
}
\details{
MLC-2v was amplified in the Roche Light Cycler 1.5. The the change of 
fluorescence was simultaneously monitored for the Hydrolysis probe of 
MLC-2v and EvaGreen. The primer sequences for MLC-2v were taken from 
Roediger et al. (2013). A 10 micro L qPCR reaction was composed of 250 nM 
primer (forward and reverse), qPCR Mix (according to the manufactures 
recommendations), 1 micro L template  (MLC-2v amplification product), 60 nM 
hydrolysis probe probe for MLC-2v. EvaGreen was used at 0.5 x final. The 
amplification was monitored at 68.5 degrees Celsius (elongation step).
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


Mao, F., Leung, W.-Y., Xin, X., 2007. Characterization of EvaGreen and the 
implication of its physicochemical properties for qPCR applications. 
\emph{BMC Biotechnol}. 7, 76.
}
\examples{
data(C126EG685)
tmp <- C126EG685

plot(NA,NA, xlim = c(1,40), ylim = c(min(tmp[, 2:ncol(tmp)]), 
    max(tmp[, 2:ncol(tmp)])), xlab = "Cycle", 
    ylab = "RFU (FAM)", 
    main = "Amplification monitored at \n68.5 degrees Celsius (elongation 
step)")

apply(tmp[, 2:ncol(tmp)], 2, 
      function(x) lines(tmp[1:nrow(tmp),1],x))
      
}
\keyword{ hydrolysis }
\keyword{ EvaGreen }
\keyword{ HPRT1 }
