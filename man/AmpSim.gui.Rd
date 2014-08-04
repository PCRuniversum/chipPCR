\name{AmpSim.gui}
\alias{AmpSim.gui}

\title{
Amplification Curve Simulation Graphical User Interface
}
\description{
Launches graphical user interface that allows simulating and analyzing 
amplification reactions. The function will open the GUI in a webpage
of the default browser. All parameters of the \code{\link[chipPCR]{AmpSim}} 
function can be used. In addition shows the GUI some information calculated
by the \code{\link[chipPCR]{bg.max}} in a summuary field and a plot
below the simulated amplification curve.
}
\usage{
AmpSim.gui()
}

\author{
Stefan Roediger, Michal Burdukiewicz.
}
\section{Warning }{
Any ad-blocking software may be cause of malfunctions. 
}

\examples{
\donttest{
# Run from a R console following commands
require(shiny)

# Invoke the shiny AmpSim app in the default browser
runApp(paste0(find.package("chipPCR")[1],"/AmpSim.gui"))
}
}

\seealso{
\code{\link[chipPCR]{AmpSim}}, \code{\link[chipPCR]{bg.max}}
}

\keyword{ hplot }
\keyword{ simulation }
\keyword{ shiny }
\keyword{ GUI }
\keyword{ browser }
