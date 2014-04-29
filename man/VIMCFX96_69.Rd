\name{VIMCFX96_69}
\alias{VIMCFX96_69}
\docType{data}
\title{
Amplfication Reaction Using the Bio-Rad CFX96
}
\description{
Data set of an amplfication reaction using the Bio-Rad CFX96 thermo cycler. 
The samples of Vimentin were amplified in the CFX96 as replicates according to 
Roediger et al. (2013). The qunatitiation was performed during the elongation 
step (69 deg Celsius).
}
\usage{data(VIMCFX96_69)}
\format{
  A data frame with 40 observations on the following 97 variables. The first 
  column ("Cycle") contains the number of cylces and consecutive columns 
  contain the replicates ("A1" to "H12").
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
data(VIMCFX96_60)
data(VIMCFX96_69)

T60 <- rowMeans(VIMCFX96_60[, 2:ncol(VIMCFX96_60)])
T69 <- rowMeans(VIMCFX96_69[, 2:ncol(VIMCFX96_69)])

plot(1:length(T60), T60, main = "Fluorescence at different 
      temperatures\nQuantitation in CFX96 (Bio-Rad)", xlab = "Cycle", 
      ylab = "Cycle dependent fluorescence", pch = 15, type = "b")
lines(1:length(T69), T69, pch = 19, type = "b", col = 2)
legend(1, 4500, c("Annealing (60 deg C)", "Elongation (69 deg C)"), 
	pch = c(15, 19), col = c(1,2))
}
\keyword{datasets}
\keyword{CFX96}
\keyword{Elongation}
