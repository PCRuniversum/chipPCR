\name{th.cyc}
\alias{th.cyc}
\title{
Threshold Cycle
}
\description{
\code{th.cyc} is a function to calculate the number of cycles at which 
the fluorescence exceeds a defined threshold, called the threshold cycle 
(Ct). According to the MIQE guidelines the Ct is refered to as 
quantification cycle (Cq).
}
\usage{
th.cyc(x, y, r = 2500, auto = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  is a vector containing the time or cycle values or a matrix or data frame
    containing both time or cycle values and fluorescence.
}
  \item{y}{
    is a vector containing the fluorescence values.
}
  \item{r}{
    a fluorescence value which defines the threshold.
}
  \item{auto}{
    is logical parameter which indicates if an automatic estimation of the 
threshold should be used (Note: Experimental, not save to use).
}
}
\details{
The Threshold Cycle (Ct) (Cq according to MIQE) is the cycle number at 
which the fluorescence excceds significantly a point above the baseline 
and defined threshold in a particular samples. Thus the Ct is the cycle 
when sufficient numbers of amplicons have accumulated. The \code{th.cyc} 
calculates the intersection of the user defined Ct value (\code{r}) and a 
quadratic polynom in the range of the user defined Ct value.
}

\references{
Stephen A. Bustin, Vladimir Benes, Jeremy A. Garson, Jan Hellemans, Jim 
Huggett, Mikael Kubista, Reinhold Mueller, Tania Nolan, Michael W. Pfaffl, 
Gregory L. Shipley, Jo Vandesompele, and Carl T. Wittwer. (Apr 2009). "The 
MIQE Guidelines: Minimum Information for Publication of Quantitative 
Real-Time PCR Experiments". \emph{Clin Chem}. 55 (4):611--22. 
doi:10.1373/clinchem.2008.112797. PMID 19246619
}
\author{
Stefan Roediger, Michal Burdukiewicz
}

\examples{
# Raw data from the VIMCFX96_69 data set.
# Cycles
x <- VIMCFX96_69[, 1]
# Fluoresce values
y <- VIMCFX96_69[, 2]

# Plot the raw data
plot(x, y, xlab = "Cycle", ylab = "Fluo")
# Calculate the the Ct value
res <- th.cyc(x, y, r = 2300)
lines(res[["Input"]], col = 2, lwd = 2)
# Threshold fluorescence value
abline(h = res[["Results"]][2], col = 3)
# Calculated Ct value
abline(v = res[["Results"]][1], col = 4)
}
\keyword{ MIQE }
\keyword{ threshold }
\keyword{ Ct }
\keyword{ Cq }
