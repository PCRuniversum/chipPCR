\name{plot.eff}
\alias{plot.eff}
\alias{plot,eff-method}
\alias{plot,eff,ANY-method}

\title{
Plot \code{eff} objects
}
\description{
Draws diagnostic plots of the amplification efficiency.
}
\usage{
\S4method{plot}{eff}(x, xlab = "log10(Concentration)", 
                     ylab = "Cq", main = "Efficiency Plot", 
                     trend = TRUE, res.fit = "topright", CI = FALSE, 
                     level = 0.95, type = "p", pch = 19, 
                     er.length = 0.05, col = "black")
}

\arguments{
  \item{x}{
is a \code{\linkS4class{eff}} object. 
}
  \item{xlab}{ a title for the x axis. }
  
  \item{ylab}{ a title for the y axis. }
  
  \item{main}{ an overall title for the plot. }
  
  \item{trend}{ Setting the option \code{trend = TRUE} shows the linear 
   regression line in the plot. }
  
  \item{res.fit}{ If \code{res.fit} sets where the results (goodness of fit, amplification 
  efficiency, correlation ) of the linear regression line in the plot should be plotted.
  Accepts the same location keywords as \code{\link[graphics]{legend}}}.
    
  \item{CI}{ Setting the option \code{CI = TRUE} shows the confidence
   interval lines in the plot. }

  \item{level}{ Tolerance/confidence level. }

  \item{type}{ is a graphical parameter setting the plot use lines, points 
  or both (see \code{\link{plot}}). }
  
  \item{pch}{ is a graphical parameter used to define the symbol used in 
  the plot. }
  
  \item{er.length}{ \code{length} is a graphical parameter used to define the 
  length of the error bar used in the plot. }

  \item{col}{ \code{col} is a graphical parameter used to define the color of the points
  on the plot. }
}
\details{
The plot being a result of this function is built from three subplots.
}
\author{
Stefan Roediger, Michal Burdukiewicz
}

\references{
Roediger S, Boehm A, Schimke I. Surface Melting Curve Analysis with R. 
\emph{The R Journal} 2013;5:37--53.
}

\seealso{
\code{\linkS4class{eff}}
}


\keyword{hplot}
