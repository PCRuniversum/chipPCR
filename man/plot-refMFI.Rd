\name{plot.refMFI}
\alias{plot.refMFI}
\alias{plot,refMFI-method}

\title{
Plot \code{refMFI} objects
}
\description{
Draw quick diagnostic plots of amplification reaction.
}
\usage{
\S4method{plot}{refMFI}(x, CV = FALSE, type = "p", pch = 19, length = 0.05, 
                        col = "black")
}

\arguments{
  \item{x}{
is a \code{\linkS4class{refMFI}} object. refMFI means referenced Mean Fluorescence Intensity (Roediger et al. 2013)
}
\item{CV}{ If \code{CV} is true the coefficient of variation (RSD, CV) is 
plotted. If set to \code{FALSE} the deviation as Standard Deviation or 
Median Absolute Deviation is plotted. }

\item{type}{ is a graphical parameter setting the plot use lines, points 
             or both (see \code{\link{plot}}). }

\item{pch}{ is a graphical parameter used to define the symbol used in 
            the plot. }

\item{length}{ \code{length} is a graphical parameter used to define the 
              length of the error bar used in the plot. }

\item{col}{ \code{col} is a graphical parameter used to define the length 
           of the error bar used in the plot. }
}
\details{
The plot being a result of this function is built from three subplots.
}
\author{
Stefan Roediger, Michal Burdukiewicz
}

\references{
Roediger S, Boehm A, Schimke I. Surface Melting Curve Analysis with R. 
\emph{The R Journal} 2013;5:37--53.
}

\seealso{
\code{\linkS4class{refMFI}}
}
\examples{

#step-by-step plotting refMFI object

}

\keyword{hplot}
