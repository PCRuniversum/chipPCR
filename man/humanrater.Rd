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
humanrater(x, repeats = 1, designations = c("y", "a", "n"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 is the input data.
}
  \item{repeats}{
number of repeats to rate the samples
}
  \item{designations}{
a character vector of length 3. See Details.
}
}

\details{
A user can specify the vector of designations characterizing the curve. 
Defaults are \code{y} for "yes", \code{a} for "ambiguous" and \code{n} for 
"no". New designations should be always supplied in the same sequence: 
"yes", "ambiguous", "no".
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

#check testdata for significance of amplification in two repeats
human.test1 <- humanrater(testdata, repeats = 2)

#check testdata for significance of amplification in one repeat and declare more
#finger friendly (but less obvious) designations
human.test2 <- humanrater(testdata, repeats = 1, c("q", "w", "e"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ rate }
\keyword{ human }
\keyword{ interactive }