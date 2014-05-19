\name{plot.der}
\alias{plot.der}
\alias{plot,der-method}

\title{
Plot \code{der} objects
}
\description{
Plot interpolate derivatives.
}
\usage{
\S4method{plot}{der}(x, what = 1:3, add = FALSE, legend = TRUE, 
        plot.colors = c("black", "red", "blue"), ...)
}

\arguments{
  \item{x}{
is a \code{\linkS4class{der}} object.
}
  \item{what}{
is a vector specifying what should be plotted. \code{1} means raw data, 
\code{2} means first derivative and \code{3} means second derivative. Any 
combination of mentioned values is valid, for example: c(1,2), c(1, 2, 3), 
c(2, 3). See Details and Examples.
}
  \item{add}{
is a \code{"logical"} argument. If \code{TRUE}, plot is added to existing 
plot.
}

  \item{legend}{
is a \code{"logical"} argument. If \code{TRUE}, legend is added to the 
plot.
}

  \item{plot.colors}{
is a vectors of colors used in plot. Must have length 3.
}
  \item{...}{
Arguments to be passed further to the plot function, such as graphical 
parameters.
}
}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\seealso{
\code{\linkS4class{der}}
}
\examples{

res <- inder(AmpSim(cyc = 1:40, Cq = 25))
plot(res)
#round the result of inder
plot(rounder(res))

}

\keyword{hplot}
