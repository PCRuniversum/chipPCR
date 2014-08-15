\name{plot.curves}
\alias{plot.curves}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot many curves
}
\description{
Plots many curves on one plot in separate cells allowing quick assessment.
}
\usage{
plot.curves(x, y, cyc = 1, fluo = 2:ncol(x), nrow = 4, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ is the column of a data frame for the cycle. }
  
  \item{y}{ are multiple columns of fluorescence values from a 
      \code{data.frame} (e.g., [, c(1:n)]).}
      
\item{cyc}{ is the index of column containing the cycle data. 
  Used only if \code{x} is matrix or data frame.}

  \item{fluo}{ are the columns containing the fluorescence data. 
  Used only if \code{x} is matrix or data frame.}
  
  \item{nrow}{
number of rows in plot.
}
  \item{\dots}{
additional arguments to \code{plot} function.
}
}
\details{
Blabla
}
\value{
None.
}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\examples{
plot.curves(VIMCFX96_60[, 1], VIMCFX96_60[, 2L:16], nrow = 4, type = "l")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
