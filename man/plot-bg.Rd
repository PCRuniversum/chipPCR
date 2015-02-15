\name{plot.bg}
\alias{plot.bg}
\alias{plot,bg-method}
\alias{plot,bg,ANY-method}

\title{
Plot \code{bg} objects
}
\description{
Draws diagnostic plots of amplification reaction.
}
\usage{
\S4method{plot}{bg}(x, what = 1:3, add = FALSE, indicators = TRUE, 
		    legend = TRUE, stan.labs = TRUE, 
		    plot.colors = c("black", "red", "blue"), ...)
}

\arguments{
  \item{x}{
is a \code{\linkS4class{bg}} object.
}
  \item{what}{
is a vector specifying what should be plotted. \code{1} means raw data, 
\code{2} means first derivative and \code{3} means second derivative. Any 
combination of mentioned values is valid, for example: c(1,2), c(1, 2, 3), 
c(2, 3). See Details and Examples.
}
  \item{add}{
is a \code{"logical"} argument. If \code{TRUE}, plot is added to existing 
plot. Moreover, enforces \code{indicators} = \code{FALSE}.
}
  \item{indicators}{
is a \code{"logical"} argument. If \code{FALSE}, background start, stop and 
plateau transition indication lines aren't plotted.
}
  \item{legend}{
is a \code{"logical"} argument. If \code{TRUE}, legend is added to the 
plot.
}

  \item{stan.labs}{
is a \code{"logical"} argument. If \code{TRUE}, standard axis labels 
("Cycle" and "Fluorescence") are added.
}
  \item{plot.colors}{
is a vectors of colors used in plot. Must have length 3.
}
  \item{...}{
Arguments to be passed further to the plot function, such as graphical 
parameters.
}
}

\details{
\code{plot.bg} is simplified, ready-to-use version of 
\code{\link[chipPCR]{plot.der}}, which still can be used whenever more 
flexible function is needed.
}

\author{
Stefan Roediger, Michal Burdukiewicz
}

\seealso{
\code{\linkS4class{bg}}, \code{\link[chipPCR]{plot.der}}
}
\examples{

#step-by-step plotting bg object

res <- AmpSim(cyc = 1:40, Cq = 25)
background <- bg.max(res)

plot(background)

#above is equivalent of below
plot(rounder(inder(res)), xlab = "Cycles", 
     ylab = "Fluorescence",
     pch = 20, legend = FALSE)

abline(v = slot(background, "bg.start"))
  text(slot(background, "bg.start"), 0.2, "Background start", pos = 4)
abline(v = slot(background, "bg.stop"), col = "blue")
  text(slot(background, "bg.stop"), 0.25, "Background stop", pos = 4, 
       col = "blue")
abline(v = slot(background, "amp.stop"), col = "green")
  text(slot(background, "amp.stop"), 0.3, "Plateau transition", pos = 4, 
       col = "green")
legend(4, 1, c("Raw data", "First derivative", "Second derivative"), 
       pch = rep(20, 3), col = c(1, 2, 4))

}

\keyword{hplot}
\keyword{amplification}
\keyword{quality}