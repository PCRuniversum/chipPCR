\name{summary-refMFI}
\alias{summary.refMFI}
\alias{summary,refMFI-method}

\title{
Summary \code{refMFI} objects
}
\description{
Summarize \code{\linkS4class{refMFI}} objects and access general statistics 
of amplification reaction. refMFI means referenced Mean Fluorescence 
Intensity (Roediger et al. 2013).
}
\usage{
\S4method{summary}{refMFI}(object, digits = getOption("digits") - 3, print = TRUE)
}

\arguments{
  \item{object}{
is a \code{\linkS4class{refMFI}} object.
}
  \item{digits}{
is a \code{numeric} value determining the number of decimal places. Used 
only
for printed values. See \link[base]{format}.
}
  \item{print}{
is a \code{logical} value determining if pretty summary of the object 
should be printed.
}
 
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
res <- AmpSim(cyc = 1:50, Cq = 41)
summary(inder(res), print = TRUE)
}
\keyword{manip}