\name{C60.melt}
\alias{C60.melt}
\docType{data}
\title{
Melt Curves MLC-2v and Vimentin for the qPCR Experiment C60.amp Using the 
Roche Light Cycler 1.5
}
\description{
Melting curves were continuously monitored with the Light Cycler 1.5 
(Roche). For details on sample preparation refer to 
\link[chipPCR]{C60.amp}.
}
\usage{data(C60.melt)}
\format{
  A data frame with 128 observations on the following 65 variables. Refer 
to \link[chipPCR]{C60.amp} for details of the specific samples.
  \describe{
    \item{\code{Index}}{Index of elements}
    \item{\code{Vim.0.1.T}}{Temperature}
    \item{\code{Vim.0.1.F}}{Fluorescence}
    \item{\code{Vim.0.2.T}}{Temperature}
    \item{\code{Vim.0.2.F}}{Fluorescence}
    \item{\code{Vim.1.1.T}}{Temperature}
    \item{\code{Vim.1.1.F}}{Fluorescence}
    \item{\code{Vim.1.2.T}}{Temperature}
    \item{\code{Vim.1.2.F}}{Fluorescence}
    \item{\code{Vim.2.1.T}}{Temperature}
    \item{\code{Vim.2.1.F}}{Fluorescence}
    \item{\code{Vim.2.2.T}}{Temperature}
    \item{\code{Vim.2.2.F}}{Fluorescence}
    \item{\code{Vim.3.1.T}}{Temperature}
    \item{\code{Vim.3.1.F}}{Fluorescence}
    \item{\code{Vim.3.2.T}}{Temperature}
    \item{\code{Vim.3.2.F}}{Fluorescence}
    \item{\code{Vim.4.1.T}}{Temperature}
    \item{\code{Vim.4.1.F}}{Fluorescence}
    \item{\code{Vim.4.2.T}}{Temperature}
    \item{\code{Vim.4.2.F}}{Fluorescence}
    \item{\code{Vim.5.1.T}}{Temperature}
    \item{\code{Vim.5.1.F}}{Fluorescence}
    \item{\code{Vim.5.2.T}}{Temperature}
    \item{\code{Vim.5.2.F}}{Fluorescence}
    \item{\code{Vim.6.1.T}}{Temperature}
    \item{\code{Vim.6.1.F}}{Fluorescence}
    \item{\code{Vim.6.2.T}}{Temperature}
    \item{\code{Vim.6.2.F}}{Fluorescence}
    \item{\code{Vim.7.1.T}}{Temperature}
    \item{\code{Vim.7.1.F}}{Fluorescence}
    \item{\code{Vim.7.2.T}}{Temperature}
    \item{\code{Vim.7.2.F}}{Fluorescence}
    \item{\code{MLC2v.1.1.T}}{Temperature}
    \item{\code{MLC2v.1.1.F}}{Fluorescence}
    \item{\code{MLC2v.1.2.T}}{Temperature}
    \item{\code{MLC2v.1.2.F}}{Fluorescence}
    \item{\code{MLC2v.2.1.T}}{Temperature}
    \item{\code{MLC2v.2.1.F}}{Fluorescence}
    \item{\code{MLC2v.2.2.T}}{Temperature}
    \item{\code{MLC2v.2.2.F}}{Fluorescence}
    \item{\code{MLC2v.3.1.T}}{Temperature}
    \item{\code{MLC2v.3.1.F}}{Fluorescence}
    \item{\code{MLC2v.3.2.T}}{Temperature}
    \item{\code{MLC2v.3.2.F}}{Fluorescence}
    \item{\code{MLC2v.4.1.T}}{Temperature}
    \item{\code{MLC2v.4.1.F}}{Fluorescence}
    \item{\code{MLC2v.4.2.T}}{Temperature}
    \item{\code{MLC2v.4.2.F}}{Fluorescence}
    \item{\code{MLC2v.5.1.T}}{Temperature}
    \item{\code{MLC2v.5.1.F}}{Fluorescence}
    \item{\code{MLC2v.5.2.T}}{Temperature}
    \item{\code{MLC2v.5.2.F}}{Fluorescence}
    \item{\code{MLC2v.6.1.T}}{Temperature}
    \item{\code{MLC2v.6.1.F}}{Fluorescence}
    \item{\code{MLC2v.6.2.T}}{Temperature}
    \item{\code{MLC2v.6.2.F}}{Fluorescence}
    \item{\code{MLC2v.7.1.T}}{Temperature}
    \item{\code{MLC2v.7.1.F}}{Fluorescence}
    \item{\code{MLC2v.7.2.T}}{Temperature}
    \item{\code{MLC2v.7.2.F}}{Fluorescence}
    \item{\code{MLC2v.0.1.T}}{Temperature}
    \item{\code{MLC2v.0.1.F}}{Fluorescence}
    \item{\code{MLC2v.0.2.T}}{Temperature}
    \item{\code{MLC2v.0.2.F}}{Fluorescence}
  }
}
\details{
Melting curves were continuously monitored with the Light Cycler 1.5 
(Roche).
}
\source{
Stefan Roediger, Claudia Deutschmann (BTU Cottbus - Senftenberg)
}
\references{ A Highly Versatile Microscope Imaging Technology Platform for 
the Multiplex Real-Time Detection of Biomolecules and Autoimmune 
Antibodies. S. Roediger, P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. 
Froemmel, C. Schmidt, M. Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and 
C. Schroeder. \emph{Advances in Biochemical Bioengineering/Biotechnology}. 
133:33--74, 2013. \url{http://www.ncbi.nlm.nih.gov/pubmed/22437246}

Mao, F., Leung, W.-Y., Xin, X., 2007. Characterization of EvaGreen and the 
implication of its physicochemical properties for qPCR applications. 
\emph{BMC Biotechnol}. 7, 76.
}
\examples{
## data(C60.melt)
str(C60.melt)
}
\keyword{datasets}
\keyword{Roche}