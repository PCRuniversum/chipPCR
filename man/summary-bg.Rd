\name{summary-bg}
\alias{summary.bg}
\alias{summary,bg-method}

\title{
Summary \code{bg} objects
}
\description{
Summarize \code{\linkS4class{bg}} objects and easily access results of 
\code{\link[chipPCR]{bg}} function.
}
\usage{
\S4method{summary}{bg}(object, print = TRUE)
}

\arguments{
  \item{object}{
is a \code{\linkS4class{bg}} object.
}
  \item{print}{
is a \code{logical} value determining if pretty summary of the object 
should be printed.
}
 
}


\author{
Stefan Roediger, Michal Burdukiewicz
}

\seealso{
\code{\linkS4class{bg}}
}
\examples{

res <- AmpSim(cyc = 1:40, Cq = 25)
background <- bg.max(res[, 1], res[, 2])
#just print summary
summary(background)

#assign summary to variable without printing
vals <- summary(background, print = FALSE)
print(vals)

#easily access different values
vals["FDM"]
}
\keyword{manip}
