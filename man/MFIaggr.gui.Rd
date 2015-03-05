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
be analyzed intuitively. This GUI dynamically generates reports.
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

# The code chunk below will fail if the web browser is not installed. if on UNIX platform try:
# as.vector(Sys.getenv("R_BROWSER"))
# Invoke the shiny AmpSim app in the default browser
\dontrun{
#do not execute using example(), it breaks the sequence of the plots in shiny app
MFIaggr.gui()
}

}

\seealso{
\code{\link[chipPCR]{MFIaggr}}
}

\keyword{ hplot }
\keyword{ shiny }
\keyword{ GUI }
\keyword{ browser }
