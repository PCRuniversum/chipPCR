\name{refMFI}
\docType{class}
\alias{refMFI-class}
\alias{refMFI}
\alias{show,refMFI-method}
\alias{qqline,refMFI-method}
\alias{qqnorm,refMFI-method}
\alias{show,refMFI-method}

\title{Class \code{"refMFI"}}
\description{
An S4 class containing the output \code{\link[chipPCR]{MFIaggr}} function. 
refMFI means referenced Mean Fluorescence Intensity (Roediger et al. 2013)
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"matrix"} containing the "Cycle", 
"Location" 
      (mean, median), "Deviation" (standard deviation, median absolute 
      deviation), "Coefficient of Variance" (CV, RSD) sequential in the 
      columns.}
    \item{\code{density}:}{\code{"density"} containing results of the 
density analysis.}
    \item{\code{qqnorm.data}:}{\code{"data.frame"} containing data required 
for 
       Quantile-Quantile plots.}
    \item{\code{stats}:}{ \code{"numeric"} vector containing general 
statistics.}
}
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "refMFI")}: plots background 
information. 
    See \code{\link[chipPCR]{plot.refMFI}}}
       \item{qqplot}{\code{signature(y = "refMFI")}: plots a normal QQ 
plot. 
    See \code{\link[stats]{qqnorm}}}
    \item{qqline}{\code{signature(y = "refMFI")}: adds a line to QQ plot. 
    See \code{\link[stats]{qqline}}}
            \item{plot}{\code{signature(x = "refMFI")}: plots the
    object. See \code{\link[chipPCR]{plot.refMFI}}}
        \item{show}{\code{signature(object = "refMFI")}: prints only 
\code{.Data} slot of the
    object.}
        \item{summary}{\code{signature(object = "refMFI")}: prints general 
statistics and 
    allows easy access to \code{stats} slot. See 
\code{\link[chipPCR]{summary.refMFI}}}
   }
}
\author{
Stefan Roediger, Michal Burdukiewicz
}

\references{
Roediger S, Boehm A, Schimke I. Surface Melting Curve Analysis with R. 
\emph{The R Journal} 2013;5:37--53.
}

\seealso{
\code{\link[chipPCR]{MFIaggr}}, \code{\link[chipPCR]{plot.refMFI}}, 
\code{\link[chipPCR]{summary.refMFI}}
}
\examples{
#PUT STH HERE
}

\keyword{classes}