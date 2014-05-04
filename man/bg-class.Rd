\name{bg}
\docType{class}
\alias{bg-class}
\alias{bg}

\title{Class \code{"bg"}}
\description{
A class 
}

\section{Slots}{
  \describe{
    \item{\code{d}:}{\code{"data.frame"} }
    \item{\code{d1}:}{\code{"data.frame"} }
    \item{\code{delta}:}{ \code{"numeric"} }
    \item{\code{delta1}:}{ \code{"numeric"} }
    \item{\code{bg.start}:}{ \code{"numeric"} }
    \item{\code{bg.stop}:}{ \code{"numeric"} }
    \item{\code{bg.corr}:}{ \code{"numeric"} }
    \item{\code{fluo}:}{ \code{"numeric"} }
    \item{\code{amp.stop}:}{ \code{"numeric"} }
    \item{\code{input}:}{ \code{"data.frame"} of original input data.}
}
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "bg")}: prints information about object 
    prettier than \code{show}.}
    \item{plot}{\code{signature(x = "bg")}: plots background information. See \code{\link{plot.bg}}}
   }
}
\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link{bg.max}}, \code{\link{plot}}
}
\examples{
res <- AmpSim(cyc = 1:40, Cq = 25)
tmp <- bg.max(res[, 1], res[, 2])
summary(tmp)
}

\keyword{classes}