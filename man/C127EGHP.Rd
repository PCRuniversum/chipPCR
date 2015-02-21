\name{C127EGHP}
\alias{C127EGHP}
\docType{data}
\title{
qPCR Experiment for the Amplification of MLC-2v Using the Roche Light Cycler 
1.5
}
\description{
Quantitative PCR (qPCR) with a hydrolysis probe (Cy5/BHQ2) and DNA binding dye 
(EvaGreen) (Mao et al. 2007) was performed in the Roche Light Cycler 1.5 
thermo cycler. The cycle-dependent increase of the fluorescence was 
quantified at the annealing step.
}
\usage{data(C127EGHP)}
\format{
  A data frame with 40 observations on the following 66 variables. The first columns
  ("index") contains index of a sample and second column ("Cycle") contains the number 
  of cycle. Consecutive columns EG1-EG32 contains fluorescence data for Eva Green dye.
  Consecutive columns HP1-HP32 contains data for hydrolysis probe.
}
\details{
MLC-2v was amplified in the Roche Light Cycler 1.5. The the change of 
fluorescence was simultaneously monitored for the Hydrolysis probe of MLC-2v 
and EvaGreen. The primer sequences for MLC-2v were taken from Roediger et al. 
(2013). A 10 micro L qPCR reaction was composed of 250 nM primer (forward and 
reverse), qPCR Mix (according to the manufactures recommendations), 1 micro L 
template  (MLC-2v amplification product), 60 nM hydrolysis probe probe for 
MLC-2v. EvaGreen was used at 0.5 x final. During the amplification was 
monitored 59.5 degrees Celsius.
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

Mao, F., Leung, W.-Y., Xin, X., 2007. Characterization of EvaGreen and the 
implication of its physicochemical properties for qPCR applications. 
\emph{BMC Biotechnol.} 7, 76.
}
\examples{
str(C127EGHP)
data(C127EGHP)
tmp <- C127EGHP

par(mfrow = c(2,1))
plot(NA, NA, xlim = c(1,40), ylim = c(0,10), xlab = "Cycle", 
      ylab = "Fluorescence", main = "MLC-2v qPCR - EvaGreen")
  for (i in 3:34) {
    points(tmp[, 2], tmp[, i], type = "l", col = i)
  }

plot(NA, NA, xlim = c(1,40), ylim = c(0,10), xlab = "Cycle", 
      ylab = "Fluorescence", main = "MLC-2v qPCR - Hydrolysis probe")
  for (i in 35:66) {
    points(tmp[, 2], tmp[, i], type = "l", col = i)
  }
par(mfrow = c(1,1))
}
\keyword{ datasets }
\keyword{ qPCR }
\keyword{ MLC-2v }
\keyword{ TaqMan }