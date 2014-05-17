\name{plot.refMFI}
\alias{plot.refMFI}
\alias{plot,refMFI-method}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot \code{refMFI} objects
}
\description{
Draw quick diagnostic plots of amplification reaction.
}
\usage{
\S4method{plot}{refMFI}(x, CV = FALSE, type = "p", pch = 19, length = 0.05, col = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
is a \code{\linkS4class{refMFI}} object.
}
\item{CV}{ If \code{CV} is true the coefficient of variation (RSD, CV) is 
           plotted. If set to \code{FALSE} the deviation as Standard Deviation or Median 
           Absolute Deviation is plotted. }

\item{type}{ is a graphical parameter setting the plot use lines, points 
             or both (see \code{\link{plot}}). }

\item{pch}{ is a graphical parameter used to define the symbol used in 
            the plot. }

\item{length}{ \code{length} is a graphical parameter used to define the 
              length of the error bar used in the plot. }

\item{col}{ \code{col} is a graphical parameter used to define the length 
           of the error bar used in the plot. }

}
\author{
Stefan Roediger, Michal Burdukiewicz
}

\seealso{
\code{\linkS4class{refMFI}}
}
\examples{

#step-by-step plotting refMFI object



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
