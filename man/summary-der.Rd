\name{summary-der}
\alias{summary.der}
\alias{summary-der}
\alias{summary,der-method}

\title{
Summary \code{der} objects
}
\description{
Summarize \code{\linkS4class{der}} objects and easily access results of 
\code{\link[chipPCR]{inder}}
function.
}
\usage{
\S4method{summary}{der}(object, digits = getOption("digits") - 3, print = TRUE)
}

\arguments{
  \item{object}{
is a \code{\linkS4class{der}} object.
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

\value{
A named vector of four elements. The element named \code{2nd_der} contains 
values of the second first derivative. The element named \code{SDM} 
contains approximate second derivative maximum, \code{SDm} contains 
approximate second derivative minimum, and \code{SDC} contains approximate 
second derivative center.
}

\details{
The approximate second derivative maximum (SDM) which is commonly used to 
quantify quantitative real-time PCR experiments. The SDM might also be 
useful for isothermal amplification processes. The SDM is calculated from a 
derived cubic spline. Similarly the first approximate approximate 
derivative maximum (FDM), second derivative minimum (SDm), and approximate 
second derivative center (SDC, geometric mean of SDM and SDm) are 
available. FDM, SDm and SDC values can be used to further characterize the 
amplification process.
}

\references{
Ruijter JM, Pfaffl MW, Zhao S, et al. (2013) Evaluation of qPCR curve 
analysis methods for reliable biomarker discovery: bias, resolution, 
precision, and implications. \emph{Methods} San Diego Calif 59:32--46.
}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\seealso{
\code{\linkS4class{der}}
}
\examples{

res <- AmpSim(cyc = 1:40, Cq = 25)
test.der <- inder(res)
#just print summary
summary(test.der)
#print without rounding
summary(test.der, digits = 6)

#assign summary to variable without printing and 
vals <- summary(test.der, print = FALSE)
print(vals)

#easily access different values
vals["bg.start"]
}
\keyword{manip}
