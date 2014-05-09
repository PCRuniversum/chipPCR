\name{bg}
\docType{class}
\alias{bg-class}
\alias{bg}

\title{Class \code{"bg"}}
\description{
An S4 class containing the output \code{\link{bg.max}} function.
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
    \item{\code{raw.data}:}{ \code{"data.frame"} of original input data.}
}
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "bg")}: prints information about object 
    prettier than \code{show} and allows easy access to some slots. See 
    \code{\link{summary.bg}}}
    \item{plot}{\code{signature(x = "bg")}: plots background information. 
    See \code{\link{plot.bg}}}
   }
}
\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link{bg.max}}, \code{\link{plot.bg}}, \code{\link{summary.bg}}
}
\examples{
res <- AmpSim(cyc = 1:40, Cq = 25)
tmp <- bg.max(res)
summary(tmp)
plot(tmp)
}

\keyword{classes}