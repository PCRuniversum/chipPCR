\name{bg}
\docType{class}
\alias{bg-class}
\alias{bg}
\alias{show.bg}
\alias{show,bg-method}

\title{Class \code{"bg"}}
\description{
An S4 class containing the output \code{\link[chipPCR]{bg.max}} function.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"matrix"} which columns represent respectively cycle number, 
    raw fluorescence data, first derivative and second derivative.}
    \item{\code{bg.start}:}{ \code{"numeric"} value representing start of the background range.}
    \item{\code{bg.stop}:}{ \code{"numeric"} value representing end of the background range.}
    \item{\code{bg.corr}:}{ \code{"numeric"} a value which helps to tweak on the suggested
    background value of \code{bg.max}.}
    \item{\code{fluo}:}{ \code{"numeric"} a value of fluorescence at the end of amplification.}
    \item{\code{amp.stop}:}{ \code{"numeric"} value representing end of the amplification
    .}
}
}
\section{Methods}{
  \describe{
  \item{plot}{\code{signature(x = "bg")}: plots background information. 
    See \code{\link[chipPCR]{plot.bg}}}
  \item{show}{\code{signature(object = "bg")}: prints only \code{.Data} slot of the
    object.}
    \item{summary}{\code{signature(object = "bg")}: prints information about object 
    prettier than \code{show} and allows easy access to some slots. See 
    \code{\link[chipPCR]{summary.bg}}}
    
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