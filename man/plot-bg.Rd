\name{plot}
\alias{plot.bg}
\alias{plot,bg-method}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot \code{bg} objects
}
\description{
Draw quick diagnostic plots of amplification reaction.
}
\usage{
\S4method{plot}{bg}(x, what = 1:3, add = FALSE, indicators = TRUE, legend = TRUE, 
        stan.labs = TRUE, plot.colors = c("black", "red", "blue"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
is a \code{\linkS4class{bg}} object.
}
  \item{what}{
is a vector specifying what should be plotted. \code{1} means raw data, \code{2} means
first derivative and \code{3} means second derivative. Any combination of mentioned 
values is valid, for example: c(1,2), c(1, 2, 3), c(2, 3). See Details and Examples.
}
  \item{add}{
is a \code{"logical"} argument. If \code{TRUE}, plot is added to existing plot. Moreover, 
enforces \code{indicators} = \code{FALSE}.
}
  \item{indicators}{
is a \code{"logical"} argument. If \code{FALSE}, background start, stop and plateau 
transition indication lines aren't plotted.
}
  \item{legend}{
is a \code{"logical"} argument. If \code{TRUE}, legend is added to the plot.
}

  \item{stan.labs}{
is a \code{"logical"} argument. If \code{TRUE}, standard axis labels ("Cycle" and
"Fluorescence") are added.
}
  \item{plot.colors}{
is a vectors of colors used in plot. Must have length 3.
}
  \item{...}{
Arguments to be passed further to the plot function, such as graphical parameters.
}
}
\author{
Stefan Roediger, Michal Burdukiewicz
}

\seealso{
\code{\linkS4class{bg}}
}
\examples{

#step-by-step plotting bg object

res <- AmpSim(cyc = 1:40, Cq = 25)
background <- bg.max(res)

plot(background)

#above is equivalent of below
plot(res, xlim = c(1, 40), ylim = c(-0.1, 1), xlab = "Cycles", 
     ylab = "Fluorescence",
     type = "b", pch = 20)

points(slot(background, "d"), col = "red", type = "b", pch = 20)
points(slot(background, "d1"), col = "blue", type = "b", pch = 20)
abline(v = slot(background, "bg.start"))
  text(slot(background, "bg.start"), 0.2, "Background start", pos = 4)
abline(v = slot(background, "bg.stop"), col = "blue")
  text(slot(background, "bg.stop"), 0.25, "Background stop", pos = 4, col = "blue")
abline(v = slot(background, "amp.stop"), col = "green")
  text(slot(background, "amp.stop"), 0.3, "Plateau transition", pos = 4, col = "green")
legend(4, 1, c("Raw data", "First derivative", "Second derivative"), 
       pch = rep(20, 3), col = c(1, 2, 4))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
