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

# Example
# Use of MFIaggr to test for heteroskedasticity using the Breusch-Pagan 
# test. The data were aggregated with the MFIaggr function and assigned to
# the object res. The standard deviation was transformed to the variance.
# The plot shows the cycle dependent variance.
# First cycles 1 to 10 of 96 qPCR replicate amplification curves were
# analyzed. Next the cycles 1 to 40 of the same amplification curve data 
# were analyzed. The Breusch-Pagan confirmed the heteroskedasticity in the
# amplification curve data.

par(mfrow = c(1,2), bty = "n")
res <- MFIaggr(VIMCFX96_60[, 1], VIMCFX96_60[, 2:ncol(VIMCFX96_60)], 
	       llul = c(1,10))
head(res)
plot(res[, 1], res[, 3]^2, xlab = "Cycle", ylab = "Variance of refMFI", 
     xlim = c(1,10), main = "ROI from Cycle 1 to 10", pch = 19, type = "b")
abline(v = c(1,10), col = "grey", lty = 2, lwd = 2)
legend("top", paste0("Breusch-Pagan test p-value: \n", format(summary(res)[5], 
       digits = 2)), bty = "n")

res <- MFIaggr(VIMCFX96_60[, 1], VIMCFX96_60[, 2:ncol(VIMCFX96_60)], 
	       llul = c(1,40))
head(res)
plot(res[, 1], res[, 3]^2, xlab = "Cycle", ylab = "Variance of refMFI", 
     main = "ROI from Cycle 1 to 40", pch = 19, type = "b")
abline(v = c(1,40), col = "grey", lty = 2, lwd = 2)
legend("top", paste0("Breusch-Pagan test p-value: \n", format(summary(res)[5], 
       digits = 2)), bty = "n")

}

\keyword{ deviation }
\keyword{ fluorescence }
\keyword{ deviation }
\keyword{ fluorescence }
\keyword{ distribution }