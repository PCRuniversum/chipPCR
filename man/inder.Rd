\name{inder}
\alias{inder}
\alias{inder.numeric}
\alias{inder.matrix}
\alias{inder.data.frame}
\alias{inder,numeric,numeric-method}
\alias{inder,matrix,missing-method}
\alias{inder,data.frame,missing-method}

\title{
Interpolate derivatives
}
\description{
A function \code{inder} ("in" + "der" = interpolate derivatives)) for 
interpolating first and second derivatives using the five-point stencil. 
Therefore this function can be used to estimate the Cq (cycle of 
quantification) of an amplification curve. First positive derivative also 
known as First Derivative Maximum (FDM) and the Second Derivative Maximum 
(SDM) are calculated this way (Ruijter et al. 2013). However, from the 
mathematical point of view it can also be used to calculate the melting 
point for melting curve analysis (compare Roediger et al. 2013) provided 
that the sign of the derivative is changed.
}
\usage{
inder(x, y, Nip = 4, logy = FALSE, smooth.method = "spline")
}

\arguments{
  \item{x}{
is a \code{numeric} vector of independent variable or data frame/matrix 
containing
abscissa and ordinate values.
}

  \item{y}{
is a vector of dependent variable. Omitted if \code{x} is data frame or 
matrix.
}
  \item{Nip}{
is a value which defines how often an interpolation takes place at n 
equally 
spaced points spanning the interval (default 4). \code{Nip} is a such a 
resolution. A high \code{Nip} may improve the precision. \code{Nip}s less 
than 
2 and higher than 20 are not meaningful for conventional qPCR with up to 
50 cycles. However, higher \code{Nip}s might be appropriate for isothermal
amplification reactions.
}

  \item{logy}{
If \code{logy} is TRUE than a semi-decadic log scale graph (corresponds to 
the 
linear phase) to illustrate the exponential dynamic of the qPCR 
amplification
is used.
}

  \item{smooth.method}{
a character vector of length 1 or \code{NNULL} defining smoothing method used. Currently 
accepted character values: "spline", "supsmu". If \code{NULL}, smoothing is not performed.
}
}
\details{
The function \code{y = f(x)} is numerically derived by five-point stencil. 
This method do not require any assumptions regarding the function \code{f}.

A smoothing procedure greatly enhances calculating derivative calculation. \code{inder}
uses two smoothing algorithms best suited for this approach. A smoothing can
be ommited by setting \code{smooth.method} to \code{NULL}, which is advisable in case of 
the data already smoothed.
}
\value{
An object of \code{\linkS4class{der}} class containing smoothed original 
data.
}
\references{
Ruijter JM, Pfaffl MW, Zhao S, et al. (2013) Evaluation of qPCR curve 
analysis methods for reliable biomarker discovery: bias, resolution, 
precision, and implications. \emph{Methods} San Diego Calif 59:32--46.

Surface Melting Curve Analysis with R. S. Roediger, A. Boehm and I. 
Schimke. \emph{The R Journal}. 5(2):37--52, 2013. 
\url{http://journal.r-project.org}
}
\author{
Stefan Roediger, Michal Burdukiewicz
}

\examples{
# First example
# Derive sinus
x <- 1:100/10
y <- sin(x)
ders <- inder(x, y)
plot(x, y, type = "l")
lines(ders[, "x"], ders[, "d1y"], col = "red")
lines(ders[, "x"], ders[, "d2y"], col = "green")
legend("topright", c("f(x)", "f'(x)", "f''(x)"), lty = 1, col = c("black", 
	"red", "green"))

# Second example
# Determine the approximate second derivative maximum
# for a qPCR experiment. SDM, 
isPCR <- AmpSim(cyc = 1:40)

res <- inder(isPCR)

plot(isPCR, xlab = "Cycle", ylab = "RFU", ylim = c(-0.15,1),
     main = "Approximate Second Derivative Maximum (SDM)", 
     type = "b", pch = 20)

  lines(res[, "x"], res[, "d1y"], col = "blue")
  lines(res[, "x"], res[, "d2y"], col = "red")
  summ <- summary(res, print = FALSE)
  abline(v = c(summ["SDM"], summ["SDm"], summ["SDC"]), col = c(3,4,5))
  text(summ["SDM"], 0.5, paste0("SDM ~ ", round(summ["SDM"], 2)), 
       cex = 1.5, col = 3)
  text(summ["SDC"], 0.7, paste0("SDC ~ ", round(summ["SDC"], 2)), 
       cex = 1.5, col = 5)
  text(summ["SDm"], 0.9, paste0("SDm ~ ", round(summ["SDm"], 2)), 
       cex = 1.5, col = 4)
  text(summ["FDM"] + 10, 0.65, paste("FDM ~ ", round(summ["FDM"], 2)), 
       cex = 1.5, col = 1)
       
  legend(1, 1, c("raw", "first derivative", "second derivative"), 
	 col = c(1,4,2), lty = c(2,1,1), cex = 1.2)
}

\keyword{ math }
\keyword{ derivative }
\keyword{ SDM }
\keyword{ FDM }
\keyword{ methods }