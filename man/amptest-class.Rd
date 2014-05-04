\name{amptest}
\docType{class}
\alias{amptest-class}
\alias{amptest}

\title{Class \code{"amptest"}}
\description{
A class 
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"numeric"} is a vector containing the fluorescence values.}
    \item{\code{decision}:}{\code{"integer"} FIX ME}
    \item{\code{noiselevel}:}{ \code{"numeric"} threshold for a significant amplification 
    signal.}
    \item{\code{background}:}{range of the background signal in the amplification curve.}
}
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "amptest")}: ... }
    \item{show}{\code{signature(object = "amptest")}: ... }
   }
}
\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link{amptester}}
}
\examples{
cyc <- seq(1, cycno <- 35, 1)
b <- -14.29; c <- 0.18; d <- 39.18; e <- 19.14
fluo.pos <- c + (d - c)/(1 + exp(b * (log(cyc) - log(e))))
ampt <- amptester(fluo.pos, background = c(0, 2))
summary(ampt)
}

\keyword{classes}