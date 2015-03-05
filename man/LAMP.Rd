\name{LAMP}
\alias{LAMP}
\docType{data}
\title{
real-time loop-mediated isothermal amplification (LAMP) by Tanner et al. (2012)
}
\description{
Real-time amplification plot of a Loop-mediated isothermal amplification 
(LAMP) by Tanner et al.(2012) in a CFX96 Real Time System (Bio-Rad Laboratories, 
Hercules, CA, USA).
}
\usage{data("LAMP")}
\format{
  A data frame with 115 observations on the following 2 variables.
  \describe{
    \item{\code{Time}}{Time in minutes}
    \item{\code{Normalized.RFU}}{RFU (Normalized)}
  }
}
\details{
The data were digitized as described by Poisot (2011). The image for 
data extraction was taken from Figure 4B by Tanner et al.(2012). For further 
experimental and technical details refer to Tanner et al.(2012).
}
\source{
The original data were taken from Figure 4B by Tanner et al.(2012).
}
\references{
Poisot, T. (2011). The digitize Package: Extracting Numerical Data from 
Scatterplots. \emph{The R Journal} 3, 25--26.

Tanner, N.A., Zhang, Y., and Evans, T.C. (2012). Simultaneous multiple target 
detection in real-time loop-mediated isothermal amplification. 
\emph{BioTechniques} 53, 81--89.
}
\examples{
plot(LAMP, main = "LAMP by Tanner et al.(2012)", xlab = "Time (minutes)", 
       ylab = "RFU (Normalized)", col = "red", type = "l")
abline(h = 0.05, lty = 2, col = "grey")
legend("bottomright", c("E. coli (Cy5)"), col = "red", pch = 19)
}
\keyword{datasets}
\keyword{LAMP}
\keyword{isothermal}