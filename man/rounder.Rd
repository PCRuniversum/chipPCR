\name{rounder}
\alias{rounder}

\title{
Round der objects
}
\description{
Function \code{\link[chipPCR]{inder}} calculates numeric derivatives on 
smoothed data which results in data points not observable in reality. The 
\code{rounder} function averages such result to the real values of cycle 
number. }

\usage{
rounder(object, cyc = 1)
}

\arguments{
  \item{object}{
a class \code{\linkS4class{der}} object.
}
  \item{cyc}{
a column containing cycle numbers after smoothing. In case of objects 
created by the \code{\link[chipPCR]{inder}} function it is the first 
column.
}
}
\value{
An object of the class \code{\linkS4class{der}} containing cycle number and 
averaged values of the fluorescence, first derivative and second 
derivative.
}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\seealso{
\code{\link[chipPCR]{inder}}, \code{\linkS4class{der}}
}
\examples{
isPCR <- AmpSim(cyc = 1:40)
res <- inder(isPCR)
rd <- rounder(res)
plot(rd)

}
\keyword{ manip }

