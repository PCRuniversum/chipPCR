\name{lm.coefs}
\alias{lm.coefs}
\title{
Compute linear model coefficients
}
\description{
Computes linear model using the robust linear regression.
}
\usage{
lm.coefs(x, y, method.reg)
}
\arguments{
  \item{x}{
a vector of ordinate values.
}
  \item{y}{
a vector of abscissa values.
}
  \item{method.reg}{
  defines the method ("rfit", "lmrob", "rq", "least") for the linear 
regression.
}
}
\details{
\code{lm.coefs} is a convenient wrapper around few functions performing 
normal (least squares) and robust linear regression. If the robust linear 
regression is impossible, \code{lm.coefs} will give a warning and perform linear 
regression using the least squares method.
This function can be used to calculate the background of an amplification
curve. The coefficients of the analysis can be used for a trend based
correction of the entire data set.
}
\value{
A data frame with one column and two rows representing coefficients of the linear
model.
}

\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link[quantreg]{rq}}, \code{\link[Rfit]{rfit}}, 
\code{\link[stats]{lm}}, \code{\link[robustbase]{lmrob}}
}
\examples{
plot(VIMCFX96_69[, 1], VIMCFX96_69[, 2], type = "l", xlab = "Cycle", 
     ylab = "Fluorescence")
rect(1,0,10,5000)
method <- c("lmrob", "rq", "least", "rfit")
for (i in 1:4) {
  tmp <- lm.coefs(VIMCFX96_69[1:10, 1], VIMCFX96_69[1:10, 2], 
		  method.reg = method[i])
  abline(a = tmp[1, 1], b = tmp[2, 1], col = i + 1, lwd = 1.5)
}
legend(2, 3000, c("Data", "lmrob", "rq", "least", "rfit"), lty = 1, col = 1:5, 
       cex = 1.5)
}

\keyword{ models }
\keyword{ robust }
\keyword{ regression }
\keyword{ MM-estimator }
\keyword{ quantile }
