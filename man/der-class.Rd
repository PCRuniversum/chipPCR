\name{der}
\docType{class}
\alias{der-class}
\alias{show.der}
\alias{show,der-method}

\title{Class \code{"der"}}
\description{
An S4 class containing the output \code{\link[chipPCR]{inder}} function.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"matrix"} is a matrix containing smoothed data as well as
    the first and second derivative.}
    \item{\code{method}:}{\code{"character"} used method of smoothing.}
}
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "der")}: calculates and prints approximate first
    derivative maximum, second derivative maximum, second derivative minimum and second
    derivative center. See \code{\link[chipPCR]{summary.der}}.}
    \item{show}{\code{signature(object = "der")}: prints only \code{.Data} slot of the
    object.}
   }
}

\details{
Blabla
}
\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link[chipPCR]{inder}}
}
\examples{
pcr <- AmpSim(cyc = 1:40)
res <- inder(pcr[, 1], pcr[, 2])
sums <- summary(res)
sums
}

\keyword{classes}