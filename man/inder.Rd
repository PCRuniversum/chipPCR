\name{inder}
\alias{inder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpolate derivatives
}
\description{
A function \code{inder} ("in" + "der" = interpolate derivatives)) for 
interpolating first and second derivatives using the five-point 
stencil. Therefore this function can be used to estimate the Cq (cycle of 
quantification) of an amplification curve. But it can also be used to calculate
the first positive derivative aka melting point.
}
\usage{
inder(x, y, Nip = 4, logy = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
is a vector of independent variable.
}

  \item{y}{
is a vector of dependent variable.
}
  \item{Nip}{
is a value which defines how often an interpolation takes place at n equally 
spaced points spanning the interval (default 4). \code{Nip} is a such a 
resolution. A high \code{Nip} may improve the precision. \code{Nip}s less than 
2 and higher than 20 are not meaningful.
}

  \item{logy}{
If \code{logy} is TRUE than a semi-decadic log scale graph (corresponds to the 
linear phase) to illustrate the exponential dynamic of the qPCR amplification
is used.
}
}
\details{
The function \code{y = f(x)} is numerically derived by five-point stencil. This 
method do not require any assumptions regarding the function \code{f}. The 
function \code{inder} calculates the approximate second derivative maximum 
(SDM) which is commonly used to quantify quantitative real-time PCR 
experiments. The SDM might also be useful for isothermal amplification 
processes. The SDM is calculated from a derived cubic spline. Similarly the 
first approximate approximate derivative maximum (FDM), second derivative 
minimum (SDm), and approximate second derivative center (SDC, geometric mean 
of SDM and SDm) are available. FDM, SDm and SDC values can be used to further 
characterize the amplification process.
}
\value{
A list of four elements. The element named \code{x} contains smoothed 
independent variable. The element named \code{y} contains smoothed dependent 
variable. The element named \code{1st_der} contains values of the first 
derivative. The element named \code{2nd_der} contains values of the second 
first derivative. The element named \code{SDM} contains approximate second 
derivative maximum, \code{SDm} contains approximate second derivative minimum, 
and \code{SDC} contains approximate second derivative center.
}
\references{
Ruijter JM, Pfaffl MW, Zhao S, et al. (2013) Evaluation of qPCR curve analysis 
methods for reliable biomarker discovery: bias, resolution, precision, and 
implications. \emph{Methods} San Diego Calif 59:32--46.
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
lines(ders[["x"]], ders[["1st_der"]], col = "red")
lines(ders[["x"]], ders[["2nd_der"]], col = "green")
legend("topright", c("f(x)", "f'(x)", "f''(x)"), lty = 1, col = c("black", 
	"red", "green"))

# Second example
# Determine the approximate second derivative maximum
# for a qPCR experiment. SDM, 
isPCR <- AmpSim(cyc = c(1:40))

res <- inder(isPCR[, 1], isPCR[, 2])

plot(isPCR, xlab = "Cycle", ylab = "RFU", ylim = c(-0.15,1),
     main = "Approximate Second Derivative Maximum (SDM)", 
     type = "b", pch = 20)

  lines(res[["x"]], res[["1st_der"]], col = "blue")
  lines(res[["x"]], res[["2nd_der"]], col = "red")
  abline(v = c(res[["SDM"]], res[["SDm"]], res[["SDC"]]), col = c(3,4,5))
  text(res[["SDM"]], 0.5, paste("SDM ~ ", round(res[["SDM"]], 2)), 
       cex = 1.5, col = 3)
  text(res[["SDC"]], 0.7, paste("SDC ~ ", round(res[["SDC"]], 2)), 
       cex = 1.5, col = 5)
  text(res[["SDm"]], 0.9, paste("SDm ~ ", round(res[["SDm"]], 2)), 
       cex = 1.5, col = 4)
  text(res[["FDM"]] + 10, 0.65, paste("FDM ~ ", round(res[["FDM"]], 2)), 
       cex = 1.5, col = 1)
       
  legend(1, 1, c("raw", "first derivative", "second derivative"), 
	 col = c(1,4,2), lty = c(2,1,1), cex = 1.2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
\keyword{ derivative }
\keyword{ SDM }
\keyword{ FDM }
