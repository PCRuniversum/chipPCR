\name{normalizer}
\alias{normalizer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalize data
}
\description{
Normalizes data using few possible methods.
}
\usage{
normalizer(y, method.norm = "none", qnL = 0.03)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
s a vector containing the fluorescence values.
}
  \item{method.norm}{
  is a argument to use a "none", "minmax", "max", "lugn", or
	"zscore" normalization.
}
  \item{qnL}{
  is the quantile to be used for the quantile normalization. Ignored if \code{method.norm}
  is not euqal to "luqn".
}
}
\details{
The parameter \code{qnL} is 
a user defined quantile which is used for the quantile normalization. A 
quantile normalization herein refers to an approach which is less prone to 
outliers than a normalization based on the minimum and the maximum of an 
amplification curve. 

The dB (decibel) as measure of the ratio between the fluorescence of the 
lowest and highest signal. The dB is defined as ten times the decadic 
logarithm of the their ratio. An example application is shown below.

\deqn{L_{dB} = 10 * \log_{10}(abs(\frac{quantile(RFU, 1 - p)}{quantile(RFU, p)}))}
}
\value{
A vector of normalized fluorescence values.
}

\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link{CPP}}
}
\examples{
normalizer(C17[2:50, 1], "minmax")
}

\keyword{ manip }

