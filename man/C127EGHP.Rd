\name{C127EGHP}
\alias{C127EGHP}
\docType{data}
\title{
qPCR Experiment for the Amplification of MLC-2v Using the Roche Light Cycler 
1.5
}
\description{
Quantitive PCR (qPCR) with a hydrolysis probe (Cy5/BHQ2) and DNA binding dye 
(EvaGreen) (Mao et al. 2007) was performed in the Roche Light Cycler 1.5 
thermo cycler. The cycle-dependent increase of the fluorescence was 
quantified at the annealing step.
}
\usage{data(C127EGHP)}
\format{
  A data frame with 40 observations on the following 66 variables.
  \describe{
    \item{\code{index}}{Index of sample}
    \item{\code{Cycle}}{Cycles}
    \item{\code{EG1}}{Eva Green}
    \item{\code{EG2}}{Eva Green}
    \item{\code{EG3}}{Eva Green}
    \item{\code{EG4}}{Eva Green}
    \item{\code{EG5}}{Eva Green}
    \item{\code{EG6}}{Eva Green}
    \item{\code{EG7}}{Eva Green}
    \item{\code{EG8}}{Eva Green}
    \item{\code{EG9}}{Eva Green}
    \item{\code{EG10}}{Eva Green}
    \item{\code{EG11}}{Eva Green}
    \item{\code{EG12}}{Eva Green}
    \item{\code{EG13}}{Eva Green}
    \item{\code{EG14}}{Eva Green}
    \item{\code{EG15}}{Eva Green}
    \item{\code{EG16}}{Eva Green}
    \item{\code{EG17}}{Eva Green}
    \item{\code{EG18}}{Eva Green}
    \item{\code{EG19}}{Eva Green}
    \item{\code{EG20}}{Eva Green}
    \item{\code{EG21}}{Eva Green}
    \item{\code{EG22}}{Eva Green}
    \item{\code{EG23}}{Eva Green}
    \item{\code{EG24}}{Eva Green}
    \item{\code{EG25}}{Eva Green}
    \item{\code{EG26}}{Eva Green}
    \item{\code{EG27}}{Eva Green}
    \item{\code{EG28}}{Eva Green}
    \item{\code{EG29}}{Eva Green}
    \item{\code{EG30}}{Eva Green}
    \item{\code{EG31}}{Eva Green}
    \item{\code{EG32}}{Eva Green}
    \item{\code{HP1}}{Hydrolysis probe}
    \item{\code{HP2}}{Hydrolysis probe}
    \item{\code{HP3}}{Hydrolysis probe}
    \item{\code{HP4}}{Hydrolysis probe}
    \item{\code{HP5}}{Hydrolysis probe}
    \item{\code{HP6}}{Hydrolysis probe}
    \item{\code{HP7}}{Hydrolysis probe}
    \item{\code{HP8}}{Hydrolysis probe}
    \item{\code{HP9}}{Hydrolysis probe}
    \item{\code{HP10}}{Hydrolysis probe}
    \item{\code{HP11}}{Hydrolysis probe}
    \item{\code{HP12}}{Hydrolysis probe}
    \item{\code{HP13}}{Hydrolysis probe}
    \item{\code{HP14}}{Hydrolysis probe}
    \item{\code{HP15}}{Hydrolysis probe}
    \item{\code{HP16}}{Hydrolysis probe}
    \item{\code{HP17}}{Hydrolysis probe}
    \item{\code{HP18}}{Hydrolysis probe}
    \item{\code{HP19}}{Hydrolysis probe}
    \item{\code{HP20}}{Hydrolysis probe}
    \item{\code{HP21}}{Hydrolysis probe}
    \item{\code{HP22}}{Hydrolysis probe}
    \item{\code{HP23}}{Hydrolysis probe}
    \item{\code{HP24}}{Hydrolysis probe}
    \item{\code{HP25}}{Hydrolysis probe}
    \item{\code{HP26}}{Hydrolysis probe}
    \item{\code{HP27}}{Hydrolysis probe}
    \item{\code{HP28}}{Hydrolysis probe}
    \item{\code{HP29}}{Hydrolysis probe}
    \item{\code{HP30}}{Hydrolysis probe}
    \item{\code{HP31}}{Hydrolysis probe}
    \item{\code{HP32}}{Hydrolysis probe}
  }
}
\details{
MLC-2v was amplified in the Roche Ligth Cycler 1.5. The the change of 
fluorescence was simultaniously monitored for the Hydrolysis probe of MLC-2v 
and EvaGreen. The primer sequences for MLC-2v were taken from Roediger et al. 
(2013). A 10 micro L qPCR reaction was composed of 250 nM primer (forward and 
reverse), qPCR Mix (accordingt to the manufactures recommendations), 1 micro L 
template  (MLC-2v amplification product), 60 nM hydorlysis probe probe for 
MLC-2v. EvaGreen was used at 0.5x final. During the amplification was 
monitored 59.5 degree Celsius.
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