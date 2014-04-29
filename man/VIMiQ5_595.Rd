\name{VIMiQ5_595}
\alias{VIMiQ5_595}
\docType{data}
\title{
Amplfication Reaction Using the Bio-Rad iQ5
}
\description{
Data set of an amplfication reaction using the Bio-Rad iQ5 thermo cycler. The 
samples of Vimentin were amplified in the iQ5 as replicates according to 
Roediger et al. (2013). The qunatitiation was performed during the annealing 
step (59.5 deg Celsius).
}
\usage{data(VIMiQ5_595)}
\format{
  A data frame with 40 observations on the following 97 variables. The first 
  column ("Cycles") contains the number of cylces and consecutive columns 
  contain the replicates ("A01" to "H12").
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
}
\examples{
T595 <- rowMeans(VIMiQ5_595[, 2:ncol(VIMiQ5_595)])
T685 <- rowMeans(VIMiQ5_685[, 2:ncol(VIMiQ5_685)])

plot(1:length(T595), T595, main = "Fluorescence at different 
    temperatures\nQuantitation in iQ5 (Bio-Rad)", xlab = "Cycle", 
    ylab = "Cycle dependent fluorescence", pch = 15, type = "b")
lines(1:length(T685), T685, pch = 19, type = "b", col = 2)
legend(1, 10000, c("Annealing (59.5 deg C)", "Elongation (68.5 deg C)"), 
	pch = c(15, 19), col = c(1,2))
}
\keyword{datasets}
\keyword{iQ5}
\keyword{Annealing}