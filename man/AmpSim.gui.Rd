\name{AmpSim.gui}
\alias{AmpSim.gui}

\title{
Amplification Curve Simulation Graphical User Interface
}
\description{
Launches graphical user interface that allows simulating and analyzing 
amplification reactions. This function will open the GUI in a webpage
of the default browser. All parameters of the \code{\link[chipPCR]{AmpSim}} 
function can be used. In addition to this, the GUI shows some information calculated
by the \code{\link[chipPCR]{bg.max}} in a summary field and a plot
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
# The code chunk below will fail if the web browser is not installed. if on UNIX platform try:
# as.vector(Sys.getenv("R_BROWSER"))
# Invoke the shiny AmpSim app in the default browser
\dontrun{
#do not execute using example(), it breaks the sequence of the plots in shiny app
AmpSim.gui()
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
