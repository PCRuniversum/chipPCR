\name{amptester.gui}
\alias{amptester.gui}

\title{
Amplification Test Graphical User Interface
}
\description{
Something
}
\usage{
amptester.gui()
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
runApp(paste0(find.package("chipPCR")[1],"/amptester.gui"))
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
