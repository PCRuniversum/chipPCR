\name{CD74}
\alias{CD74}
\docType{data}
\title{
Quantitive PCR with a hydrolysis probe and DNA binding dye
}
\description{
Quantitive PCR (qPCR) with a hydrolysis probe and DNA binding dye (EvaGreen) 
(Mao et al. 2007) was performed in the VideoScan heating cooling unit. The 
cycle-dependent increase of the fluorescence was quantified at three different 
temperatures in order to estimate temperature dependent effects.
}
\usage{data(CD74)}
\format{
  A data frame with 60 observations on the following 19 variables. refMFI, 
  fluorescence. Dilution A, 1x; Dilution B, 1:10; Dilution B, 1:100.
  \describe{
      \item{\code{Cycle}}{PCR cycles}
      \item{\code{EG.30.A}}{refMFI at 30 deg Celsius, EvaGreen, Dilution A}
      \item{\code{EG.59.5.A}}{refMFI at 59.5 deg Celsius, EvaGreen, Dilution A}
      \item{\code{EG.68.5.A}}{refMFI at 68.5 deg Celsius, EvaGreen, Dilution A}
      \item{\code{HP.30.A}}{refMFI at 30 deg Celsius, hydrolysis probe, 
			    Dilution A}
      \item{\code{HP.59.5.A}}{refMFI at 59.5 deg Celsius, hydrolysis probe, 
			      Dilution A}
      \item{\code{HP.68.5.A}}{refMFI at 68.5 deg Celsius, hydrolysis probe, 
			      Dilution A}
      \item{\code{EG.30.B}}{refMFI at 30 deg Celsius, EvaGreen, Dilution B}
      \item{\code{EG.59.5.B}}{refMFI at 59.5 deg Celsius, EvaGreen, Dilution B}
      \item{\code{EG.68.5.B}}{refMFI at 68.5 deg Celsius, EvaGreen, Dilution B}
      \item{\code{HP.30.B}}{refMFI at 30 deg Celsius, hydrolysis probe, 
			    Dilution B}
      \item{\code{HP.59.5.B}}{refMFI at 59.5 deg Celsius, hydrolysis probe, 
			      Dilution B}
      \item{\code{HP.68.5.B}}{refMFI at 68.5 deg Celsius, hydrolysis probe, 
			      Dilution B}
      \item{\code{EG.30.C}}{refMFI at 30 deg Celsius, EvaGreen, Dilution C}
      \item{\code{EG.59.5.C}}{refMFI at 59.5 deg Celsius, EvaGreen, Dilution C}
      \item{\code{EG.68.5.C}}{refMFI at 68.5 deg Celsius, EvaGreen, Dilution C}
      \item{\code{HP.30.C}}{refMFI at 30 deg Celsius, hydrolysis probe, 
			    Dilution C}
      \item{\code{HP.59.5.C}}{refMFI at 59.5 deg Celsius, hydrolysis probe, 
			      Dilution C}
      \item{\code{HP.68.5.C}}{refMFI at 68.5 deg Celsius, hydrolysis probe, 
			      Dilution C}
  }
}
\details{
The aim was to amplify MLC-2v in the VideoScan platform while the 
intercalating dye EvaGreen and a hydrolysis probe for MLC-2v were used 
simultaneously. The primer sequences for MLC-2v were taken from Roediger et 
al. (2013). The amplification was detected in solution of the 1 HCU (see 
Roediger et al. 2013 for details). A 20 micro l PCR reaction was composed of 
500 nM primer (forward and reverse), 1x Maxima Probe qPCR Master Mix 
(Fermentas), 1 micro l template (MLC-2v amplification product in different 
dilutions), 50 nM hydorlysis probe probe for MLC-2v, 0.5x EvaGreen and A. 
bidest. During the amplification, fluorescence was measured at 3 different 
temperatures, at 59.5 degree Celsius the annealing temperature, at 68.5 degree 
Celsius the elongation temperature and at 30 degree Celsius. The FAM channel 
was used to monitor EvaGreen and the Cy5 channel to monitor the MLC-2v specific 
hydrolysis probe.
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
\emph{BMC Biotechnol}. 7, 76.
}
\examples{
# First example
# Comparison of smoothers and filter on amplification curves
# Amplification curves were measured at three temperature (30, 
# 59.5, 68.5 degree Celsius) using the VideoScan HCU (see 
# Roediger et al. 2013 for details). MLC-2v was amplified.
# The change of fluorescence was monitored by the intercalating
# dye EvaGreen and hydrolsis probes.

data(CD74)
par(mfrow = c(2,1))
plot(NA, NA, xlim = c(1,30), ylim = c(0,2), xlab = "Cycle", 
     ylab = "MFI", main = "VideoScan HCU - Raw Data")
lim <- 1:30
for (j in c(2:4)) {
  for (i in seq(j,19,6)) {
    lines(CD74[lim, 1], CD74[lim, i], col = 1) 
  }
}
for (j in c(5:7)) {
  for (i in seq(j,19,6)) {
    lines(CD74[lim, 1], CD74[lim, i], col = 2) 
  }
}

plot(NA, NA, xlim = c(1,30), ylim = c(0,0.8), xlab = "Cycle", 
     ylab = "MFI", main = "VideoScan HCU - Smoothed Data")
lim <- 1:30
for (j in c(2:4)) {
  for (i in seq(j,19,6)) {
    lines(CD74[lim, 1], smoother(CD74[lim, 1], CD74[lim, i], trans = TRUE), 
	  col = 1) 
  }
}
for (j in c(5:7)) {
  for (i in seq(j,19,6)) {
    lines(CD74[lim, 1], smoother(CD74[lim, 1], CD74[lim, i], trans = TRUE), 
	  col = 2) 
  }
}
par(mfrow = c(1,1))
}
\keyword{datasets}
