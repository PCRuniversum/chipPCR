\name{MFIaggr.gui}
\alias{MFIaggr.gui}

\title{
Multiple Comparison of the Cycle Dependent Variance - Graphical User 
Interface
}
\description{
\code{MFIaggr.gui} is a Graphical User Interface based on the \code{shiny} 
package. The core if this GUI is based on the Comparison of the 
\code{\link[chipPCR]{MFIaggr}} function. The cycle dependent variance can
be analyzed intuitively. A report generation is possible from this GUI.
}
\usage{
MFIaggr.gui()
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
runApp(paste0(find.package("chipPCR")[1],"/MFIaggr.gui"))
}
}

\seealso{
\code{\link[chipPCR]{MFIaggr}}
}

\keyword{ hplot }
\keyword{ shiny }
\keyword{ GUI }
\keyword{ browser }
