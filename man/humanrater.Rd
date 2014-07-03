\name{humanrater}
\alias{humanrater}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
humanrater, a graphical interface to rate curves
}
\description{
The function \code{humanrater} is an interactive function, which can be 
used to rate a curve for a certain characteristic. \code{humanrater} draws
individual graphs of a curve and prompts an input field for the user. This
function can be used to compare the human rating and the rating of a 
machine.
}
\usage{
humanrater(x, repeats = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 is the input data.
}
  \item{repeats}{
number of repeats to rate the samples
}
}

\author{
Michal Burdukiewicz, Stefan Roediger
}

\examples{
\dontrun{
testdata <- data.frame(1:35, AmpSim(Cq = 15, noise = TRUE), 
			     AmpSim(Cq = 25, noise = TRUE), 
			     AmpSim(Cq = 35, noise = TRUE), 
			     AmpSim(Cq = 45, noise = TRUE))
humanrater(testdata)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ rate }
\keyword{ human }
