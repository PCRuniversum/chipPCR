\name{plotCurves}
\alias{plotCurves}
\title{
Plot Curves in an Orthogonal Matrix
}
\description{
Plots many curves on one plot in separate cells allowing quick assessment.
}
\usage{
plotCurves(x, y, cyc = 1, fluo = 2:ncol(x), one.plot = FALSE, 
           nrow = ceiling(sqrt(ncol(y))), CPP = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ is the column of a data frame for the cycle or data.frame/matrix with whole data.}
  
  \item{y}{ are multiple columns of fluorescence values from a 
      \code{data.frame} (e.g., [, c(1:n)]).}
      
\item{cyc}{ is the index of column containing the cycle data. 
  Used only if \code{y} is \code{NULL}.}

  \item{fluo}{ are the columns containing the fluorescence data. 
  Used only if \code{y} is \code{NULL}.}
  
    \item{one.plot}{ \code{logical}, curves may be plotted on single chart (\code{TRUE})
    or in matrix-like multiplot (\code{FALSE}).}
    
  \item{nrow}{
number of rows in plot. Applies only if \code{one.plot} is \code{FALSE}.
}
  \item{CPP}{
logical, if \code{TRUE} CPP analysis is added to a plot. Applies only if \code{one.plot} is
\code{FALSE}.
}

  \item{\dots}{
additional arguments to \code{plot} function.
}
}
\details{
\code{plotCurves} is a function for the quick assessment of amplification 
curve raw data in an orthogonal matrix.
}
\value{
None.
}

\author{
Stefan Roediger, Michal Burdukiewicz, Konstantin A. Blagodatskikh
}

\examples{
# First example
plotCurves(VIMCFX96_60[, 1], VIMCFX96_60[, 2L:16], type = "l")

# Second example
y <- VIMCFX96_60[, 2L:16]
# Introduce some missing values.
y[c(1,4,5,6,23,34), c(2,4,9,15)] <- NA
plotCurves(VIMCFX96_60[, 1], y, nrow = 4, type = "l")

# Third example
# Same as second example but the CPP option is set to TRUE.
# Noise and missing values will be removed. 

y <- VIMCFX96_60[, 2L:16]
# Introduce some missing values.
y[c(1,4,5,6,23,34), c(2,4,9,15)] <- NA
plotCurves(VIMCFX96_60[, 1], y, nrow = 4, CPP = TRUE, type = "l")

# Fourth example
plotCurves(VIMCFX96_60, y = NULL, one.plot = TRUE, type = "l")
plotCurves(VIMCFX96_60, y = NULL, one.plot = FALSE, type = "l")


}

\keyword{ hplot }
