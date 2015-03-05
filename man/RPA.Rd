\name{RPA}
\alias{RPA}
\docType{data}
\title{
  Recombinase polymerase amplification (RPA) by Lutz et al.(2009)
}
\description{
Real-time amplification plot of an Recombinase Polymerase Amplification (RPA) 
by Lutz et al.(2009) in a centrifugal microfluidic foil cartridge. 
}
\usage{data("RPA")}
\format{
  A data frame with 184 observations on the following 2 variables.
  \describe{
    \item{\code{Reaction.Time}}{Reaction Time [min]}
    \item{\code{RFU}}{Relative fluorescence units [RFU]}
  }
}
\details{
The data were digitized as described by Poisot (2011). The image for data 
extraction was taken from Figure 3b by Lutz et al.(2009). The amplification 
curve present the results of a cross-contamination test of a foil disk for a 
sample containing 2000 copies of the mecA gene. For further experimental and 
technical details refer to Lutz et al.(2009). 
}
\source{
The original data (open access under CC BY-NC-ND license) were taken from Figure 
3b by Lutz et al.(2009).
}
\references{
Poisot, T. (2011). The digitize Package: Extracting Numerical Data from 
Scatterplots. \emph{The R Journal} 3, 25--26.

Lutz, S., Weber, P., Focke, M., Faltin, B., Roth, G., Piepenburg, O., Armes, 
N., Mark, D., Zengerle, R., and von Stetten, F. (2009). Isothermal Polymerase 
Amplification in a Centrifugal Microfluidic Foil Cartridge. \emph{Procedia 
Chemistry} 1, 529--531.}
\examples{
data(RPA)
plot(RPA, main = "RPA by Lutz et al.(2009)", xlab = "Reaction Time [min]", 
     ylab = "Relative fluorescence units [RFU]", type = "l")
abline(h = 5, lty = 2, col = "grey")
}
\keyword{datasets}
\keyword{RPA}
\keyword{isothermal}