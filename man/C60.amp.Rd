\name{C60.amp}
\alias{C60.amp}
\docType{data}
\title{
qPCR Experiment for the Amplification of MLC-2v and Vimentin (as decadic 
dilutions) Using the Roche Light Cycler 1.5
}
\description{
Dilution experiment and metling curve (\code{\link[chipPCR]{C60.melt}}) for 
the human genes MLC-2v and Vimentin (see Roediger et al. 2013) using the 
Roch Light Cycler 1.5.
}
\usage{data(C60.amp)}
\format{
  A data frame with 45 observations on the following 33 variables.
  \describe{
    \item{\code{Index}}{Index of Cycles}
    \item{\code{Vim.0.1}}{Vimentin water control}
    \item{\code{Vim.0.2}}{Vimentin water control}
    \item{\code{Vim.1.1}}{Vimentin 10^-3 diluted}
    \item{\code{Vim.1.2}}{Vimentin 10^-3 diluted}
    \item{\code{Vim.2.1}}{Vimentin 10^-4 diluted}
    \item{\code{Vim.2.2}}{Vimentin 10^-4 diluted}
    \item{\code{Vim.3.1}}{Vimentin 10^-5 diluted}
    \item{\code{Vim.3.2}}{Vimentin 10^-5 diluted}
    \item{\code{Vim.4.1}}{Vimentin 10^-6 diluted}
    \item{\code{Vim.4.2}}{Vimentin 10^-6 diluted}
    \item{\code{Vim.5.1}}{Vimentin 10^-7 diluted}
    \item{\code{Vim.5.2}}{Vimentin 10^-7 diluted}
    \item{\code{Vim.6.1}}{Vimentin 10^-8 diluted}
    \item{\code{Vim.6.2}}{Vimentin 10^-8 diluted}
    \item{\code{Vim.7.1}}{Vimentin 10^-9 diluted}
    \item{\code{Vim.7.2}}{Vimentin 10^-9 diluted}
    \item{\code{MLC2v.1.1}}{MLC-2v 10^-3 diluted}
    \item{\code{MLC2v.1.2}}{MLC-2v 10^-3 diluted}
    \item{\code{MLC2v.2.1}}{MLC-2v 10^-4 diluted}
    \item{\code{MLC2v.2.2}}{MLC-2v 10^-4 diluted}
    \item{\code{MLC2v.3.1}}{MLC-2v 10^-5 diluted}
    \item{\code{MLC2v.3.2}}{MLC-2v 10^-5 diluted}
    \item{\code{MLC2v.4.1}}{MLC-2v 10^-6 diluted}
    \item{\code{MLC2v.4.2}}{MLC-2v 10^-6 diluted}
    \item{\code{MLC2v.5.1}}{MLC-2v 10^-7 diluted}
    \item{\code{MLC2v.5.2}}{MLC-2v 10^-7 diluted}
    \item{\code{MLC2v.6.1}}{MLC-2v 10^-8 diluted}
    \item{\code{MLC2v.6.2}}{MLC-2v 10^-8 diluted}
    \item{\code{MLC2v.7.1}}{MLC-2v 10^-9 diluted}
    \item{\code{MLC2v.7.2}}{MLC-2v 10^-9 diluted}
    \item{\code{MLC2v.0.1}}{MLC-2v water control}
    \item{\code{MLC2v.0.2}}{MLC-2v water control}
  }
}
\details{ 
MLC-2v and Vimentin were amplified in the Roche Ligth Cycler 1.5. 
Decadic dilutions of the input cDNA were prepared. The  change of 
fluorescence was simultaniously monitored with EvaGreen. The primer 
sequences for MLC-2v were taken from Roediger et al. (2013). A 10 micro L 
qPCR reaction was composed of 250 nM primer (forward and reverse), Roche 
qPCR Master-Mix (accordingt to the manufactures recommendations) and 1 
micro L input DNA. EvaGreen was used at 1x final. During the amplification 
was monitored 58 degree Celsius. Temperature profile:

    95 deg C for 8 minutes
    40 x {
	  95 deg C for 10 sec
	  58 deg C for 15 sec
	  69 deg C for 25 sec
    }
}
\source{
Stefan Roediger, Clauia Deutschman (BTU Cottbus - Senftenberg)
}
\references{
A Highly Versatile Microscope Imaging Technology Platform for the Multiplex 
Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. Roediger, 
P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, M. 
Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder. 
\emph{Advances in Biochemical Bioengineering/Biotechnology}. 133:33--74, 
2013. \url{http://www.ncbi.nlm.nih.gov/pubmed/22437246}

Mao, F., Leung, W.-Y., Xin, X., 2007. Characterization of EvaGreen and the 
implication of its physicochemical properties for qPCR applications. 
\emph{BMC Biotechnol}. 7, 76.
}
\examples{
## data(C60.amp)
str(C60.amp)
}
\keyword{datasets}
\keyword{Roche}