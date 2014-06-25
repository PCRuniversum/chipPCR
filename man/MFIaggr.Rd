\name{MFIaggr}
\alias{MFIaggr}
\alias{MFIaggr.numeric}
\alias{MFIaggr.matrix}
\alias{MFIaggr.data.frame}
\alias{MFIaggr,numeric,numeric-method}
\alias{MFIaggr,matrix,missing-method}
\alias{MFIaggr,data.frame,missing-method}

\title{
Multiple comparison of the cycle dependent variance of the fluorescence
}
\description{
 MFIaggr is used for a fast multiple comparison of the cycle dependent 
variance of the fluorescence. A similar tool with different scope is 
interated in the \code{\link[MBmca]{MFIerror}} function (Roediger et al. 
2013).
}
\usage{
\S4method{MFIaggr}{numeric,numeric}(x, y, cyc = 1, fluo = 2:ncol(x), 
		   RSD = FALSE, rob = FALSE, llul = c(1,10))
\S4method{MFIaggr}{matrix,missing}(x, y, cyc = 1, fluo = 2:ncol(x), 
		   RSD = FALSE, rob = FALSE, llul = c(1,10))
\S4method{MFIaggr}{data.frame,missing}(x, y, cyc = 1, fluo = 2:ncol(x), 
		   RSD = FALSE, rob = FALSE, llul = c(1,10))
}

\arguments{
  \item{x}{ is the column of a data frame for the cycle. }
  
  \item{y}{ are multiple columns of fluorescence values from a 
	    \code{data.frame} (e.g., [, c(1:n)]). Omitted if \code{x} is 
	    matrix or data frame.}

  \item{cyc}{ is the index of column containing the cycle data. 
  Used only if \code{x} is matrix or data frame.}

  \item{fluo}{ are the columns containing the fluorescence data. 
  Used only if \code{x} is matrix or data frame.}

  \item{RSD}{ Setting the option \code{RSD = TRUE} shows the relative 
standard deviation (RSD) in percent. }
  
  \item{rob}{ Using the option \code{rob} as TRUE the median and the 
median 
absolute deviation (MAD) are calculated instead of the mean and standard 
deviation. }
  
 \item{llul}{ is a parameter to define the lower and upper 
  data limit (cycle), aka region of interest (ROI) used for the density 
  and quantile plot. }
}

\value{
An object of the class \code{\linkS4class{refMFI}}. refMFI means referenced 
Mean Fluorescence Intensity (Roediger et al. 2013).
}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\references{
Roediger S, Boehm A, Schimke I. Surface Melting Curve Analysis with R. 
\emph{The R Journal} 2013;5:37--53.
}

\examples{
# First Example
# Cycle dependent variance of the refMFI using standard measures 
# (Mean, Standard Deviation (SD)).
# Use Standard Deviation (SD) in the plot

data(VIMCFX96_60)

MFIaggr(VIMCFX96_60[, 1], VIMCFX96_60[, 2:ncol(VIMCFX96_60)])

#alternative usage
MFIaggr(VIMCFX96_60)

#only second and forth column
plot(MFIaggr(VIMCFX96_60, fluo = c(2, 4)))

}

\keyword{ deviation }
\keyword{ fluorescence }
\keyword{ deviation }
\keyword{ fluorescence }
\keyword{ distribution }