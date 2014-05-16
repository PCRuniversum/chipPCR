\name{bg}
\docType{class}
\alias{bg-class}
\alias{bg}

\title{Class \code{"bg"}}
\description{
An S4 class containing the output \code{\link[chipPCR]{bg.max}} function.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"matrix"} }
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
    \code{\link[chipPCR]{summary.bg}}}
    \item{plot}{\code{signature(x = "bg")}: plots background information. 
    See \code{\link[chipPCR]{plot.bg}}}
   }
}
\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link[chipPCR]{bg.max}}, \code{\link[chipPCR]{plot.bg}}, 
\code{\link[chipPCR]{summary.bg}}
}
\examples{
res <- AmpSim(cyc = 1:40, Cq = 25)
tmp <- bg.max(res)
summary(tmp)
plot(tmp)
}

\keyword{classes}