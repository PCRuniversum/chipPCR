\name{amptester.gui}
\alias{amptester.gui}

\title{
Amplification Test Graphical User Interface
}
\description{
\code{\link[chipPCR]{amptester.gui}} is a graphical user interface for the 
\code{\link[chipPCR]{amptester}} function. This function can be used for a 
fast and convenient analysis of amplification curve data. In addition it is 
possible to analyze the Cq (quantification cycle) and to perform a report 
generation of the analyzed data. }
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

# The code chunk below will fail if the web browser is not installed. if on UNIX platform try:
# as.vector(Sys.getenv("R_BROWSER"))
# Invoke the shiny AmpSim app in the default browser
\dontrun{
#do not execute using example(), it breaks the sequence of the plots in shiny app
amptester.gui()
}
}

\seealso{
\code{\link[chipPCR]{AmpSim}}, \code{\link[chipPCR]{bg.max}}
}

\keyword{ hplot }
\keyword{ amplification }
\keyword{ shiny }
\keyword{ report }
\keyword{ analysis }
\keyword{ GUI }
\keyword{ browser }
\keyword{ Cq }

