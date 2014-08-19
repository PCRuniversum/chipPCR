\name{eff}
\docType{class}
\alias{eff-class}
\alias{eff}
\alias{show.eff}
\alias{show,eff-method}
\alias{summary.eff}
\alias{summary,eff-method}

\title{Class \code{"eff"}}
\description{
An S4 class containing the output \code{\link{effcalc}} function.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"matrix"} }
    \item{\code{amplification.efficiency}:}{ \code{"numeric"} value representing 
    amplification efficiency.}
    \item{\code{eff.stop}:}{ \code{"numeric"} .}
    \item{\code{regression}:}{ \code{"lm"} .}
    \item{\code{correlation.test}:}{ \code{"htest"}.}
}
}
\section{Methods}{
  \describe{
  \item{plot}{\code{signature(x = "eff")}: plots information. 
    See \code{\link[chipPCR]{plot.eff}}}
  \item{show}{\code{signature(object = "eff")}: prints only \code{.Data} slot of the
    object.}
    \item{summary}{\code{signature(object = "eff")}: prints information about object 
    prettier than \code{show}.}
    
   }
}
\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link{effcalc}}, \code{\link{plot.eff}},
}


\keyword{classes}