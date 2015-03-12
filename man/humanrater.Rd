\name{humanrater}
\alias{humanrater}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
humanrater, a graphical interface to rate curves
}
\description{
\code{humanrater} allows rating a curve for a certain characteristic in the interactive, 
semi-blind manner. \code{humanrater} draws
individual graphs of a curve and prompts an input field for the user. This
function can be used to compare the human rating and the rating of a 
machine.
}
\usage{
humanrater(x, cyc = 1, repeats = 1, 
           designations = list(y = "yes", a = "ambiguous", n = "not"), 
           shuffle = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 is the input data (matrix or data.frame).
}

  \item{cyc}{
is the index of column containing the cycle data.
}
  \item{repeats}{
number of repeats to rate the samples.
}
  \item{designations}{
a named list of length at least 2. See Details.
}
  \item{shuffle}{
logical, if \code{TRUE} sequence of curves is shuffled for purpose of rating.
}
  \item{...}{
additional arguments to \code{\link[graphics]{plot}} function.
}
}

\details{
A user can specify the list of designations characterizing the curve, where the names of
elements specify short designations used during rating. Defaults are \code{y} for "yes", \code{a} 
for "ambiguous" and \code{n} for "no".

The supplied designation list must have length at least two (for example "true"/"false").
}

\author{
Michal Burdukiewicz, Stefan Roediger
}

\examples{

testdata <- data.frame(1:35,
		       AmpSim(Cq = 15, noise = TRUE)[, 2], 
		       AmpSim(Cq = 25, noise = TRUE)[, 2],
		       rnorm(35),
		       AmpSim(Cq = 35, noise = TRUE)[, 2],
		       rnorm(35),
		       AmpSim(Cq = 45, noise = TRUE)[, 2])

#we strongly advise against running code below using 'example(humanrater)'
#due to the highly interactive nature of this function (it would not end
#without user's input), it is recommended to just copy lines below into R
#command line
\dontrun{
#check testdata for significance of amplification in two repeats
human.test1 <- humanrater(testdata, repeats = 2)

#check testdata for significance of amplification in one repeat and declare more
#finger friendly (but less obvious) designations
human.test2 <- humanrater(testdata, repeats = 1, list(q = "yes", w = "no"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ rate }
\keyword{ human }
\keyword{ interactive }