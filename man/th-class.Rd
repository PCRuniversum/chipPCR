\name{th}
\docType{class}
\alias{th}
\alias{th-class}
\alias{summary.th}
\alias{summary,th-method}
\alias{show.th}
\alias{show,th-method}

\title{Class \code{"th"}}
\description{
An S4 class containing the output \code{\link[chipPCR]{th.cyc}} 
function.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{\code{"matrix"} is a matrix containing the 
  		 threshold cycle and threshold fluorescence.}
    \item{\code{stats}:}{\code{"summary.lm"} contains linear model used for Ct estimation.}
    \item{\code{input}:}{\code{"matrix"} input data for linear model.}
}
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "th")}: prints summary 
		   of the object.}
    \item{show}{\code{signature(object = "th")}: prints only 
		\code{.Data} slot of the object.}
   }
}
\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link{th.cyc}}
}
\examples{
res <- th.cyc(VIMCFX96_69[, 1], VIMCFX96_69[, 3], r = 2300)
summary(res)
slot(res, "input")
}

\keyword{classes}