\name{amptest}
\docType{class}
\alias{amptest-class}
\alias{summary.amptest}
\alias{summary,amptest-method}
\alias{show.amptest}
\alias{show,amptest-method}

\title{Class \code{"amptest"}}
\description{
An S4 class containing the output \code{\link[chipPCR]{amptester}} 
function.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"numeric"} is a vector containing the 
			 fluorescence values.}
    \item{\code{decision}:}{\code{"integer"} states if the amplification 
			    is "positive" or "negative"}
    \item{\code{noiselevel}:}{ \code{"numeric"} user-defined threshold for a 
			      significant amplification signal.}
    \item{\code{noise}:}{ \code{"logical"} \code{TRUE} if noise was detected.}
    \item{\code{linearity}:}{ \code{"logical"} \code{TRUE} if data are 
				likely from a amplification curve.}
    \item{\code{background}:}{range of the background signal in the 
			      amplification curve.}
}
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "amptest")}: prints summary 
		   of the object.}
    \item{show}{\code{signature(object = "amptest")}: prints only 
		\code{.Data} slot of the object.}
   }
}
\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link[chipPCR]{amptester}}
}
\examples{
cyc <- seq(1, cycno <- 35, 1)
b <- -14.29; c <- 0.18; d <- 39.18; e <- 19.14
fluo.pos <- c + (d - c)/(1 + exp(b * (log(cyc) - log(e))))
ampt <- amptester(fluo.pos, background = c(0, 2))
summary(ampt)
}

\keyword{classes}