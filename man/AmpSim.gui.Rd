\name{AmpSim.gui}
\alias{AmpSim.gui}

\title{
Amplification Curve Simulation Graphical User Interface
}
\description{
Launches graphical user interface that allows simulating and analyzing 
amplification reactions.
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
\dontrun{
#Run from a R console following commands
require(shiny)
#Invoke the shiny AmpSim app in the default browser
runApp(paste(find.package("chipPCR"),"/AmpSim.gui", sep = ""))
}
}

\seealso{
\code{\link[chipPCR]{AmpSim}}, \code{\link[chipPCR]{bg.max}}
}

\keyword{ hplot }
