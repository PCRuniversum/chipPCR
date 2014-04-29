\name{MFIaggr}
\alias{MFIaggr}

\title{
Multiple comparison of the cycle dependent variance of the refMFI
}
\description{
 MFIaggr is used for a fast multiple comparison of the cycle dependent 
variance of the refMFI. \code{MFIaggr} returns an object of the class 
data.frame with columns ``Cycle'', ``Location'' (Mean, Median), 
``Deviation'' (Standard Deviation, Median Absolute Deviation) and 
``Coefficient of Variation''.
}
\usage{
MFIaggr(x, y, CV = FALSE, RSD = FALSE, rob = FALSE, 
	errplot = TRUE, type = "p", pch = 19, length = 0.05, 
	col = "black", llul = c(1,10))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ is the column of a data frame for the cycle. }
  
  \item{y}{ are multiple columns of fluorescence values from a 
\code{data.frame} (e.g., [, c(1:n)]). }
  
  \item{CV}{ If \code{CV} is true the coefficient of variation (RSD, CV) is 
plotted. If set to FLASE the deviation as Standard Deviation or Median 
Absolute Deviation is plotted. }
  
  \item{RSD}{ Setting the option \code{RSD = TRUE} shows the relative 
standard deviation (RSD) in percent. }
  
  \item{rob}{ Using the option \code{rob} as TRUE the median and the median 
absolute deviation (MAD) is plotted instead of the mean and standard 
deviation. }
  
  \item{errplot}{sets \code{MFIaggr()} to plot the results (default). In 
the default setting (\code{CV = FALSE}) the mean with the standard deviations 
is plotted. }

  \item{type}{ is a graphical parameter setting the plot use lines, points 
or both (see \code{\link{plot}}). }
  
  \item{pch}{ is a graphical parameter used to define the symbol used in 
the plot. }
  
  \item{length}{ \code{length} is a graphical parameter used to define the 
length of the error bar used in the plot. }

  \item{col}{ \code{col} is a graphical parameter used to define the length 
of the error bar used in the plot. }
  
 \item{llul}{ \code{llul} is a parameter to define the lower and upper 
  data limit (cycle), aka region of interest (ROI) used for the density 
  and quantile plot. }
}

\value{
  \item{res }{
      returns a \code{list} containing the "Cycle", "Location" 
      (mean, median), "Deviation" (standard deviation, median absolute 
      deviation), "Coefficient of Variance" (CV, RSD) sequential in the 
      columns, the results of the density analysis and the results of
      the Quantile-Quantile plot.}
}

\author{
Stefan Roediger
}

\references{
A Highly Versatile Microscope Imaging Technology Platform for the Multiplex 
Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. Roediger, 
P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, M. 
Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder. 
\emph{Advances in Biochemical Bioengineering/Biotechnology}. 133:33--74, 
2013. \url{http://www.ncbi.nlm.nih.gov/pubmed/22437246}

Nucleic acid detection based on the use of microbeads: a review. S. 
Roediger, C. Liebsch, C. Schmidt, W. Lehmann, U. Resch-Genger, U. Schedler, 
P. Schierack. \emph{Microchim Acta} 2014:1--18. DOI: 
10.1007/s00604-014-1243-4
}
\examples{
# First Example
# Cycle dependent variance of the refMFI using standard measures 
# (Mean, Standard Deviation (SD)).
# Use Standard Deviation (SD) in the plot

data(VIMCFX96_60)
tmp <- VIMCFX96_60
MFIaggr(tmp[, 1], tmp[, 2:ncol(tmp)])
}

\keyword{ deviation }