\name{lm.coefs}
\alias{lm.coefs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute linear model coefficients
}
\description{
Computes linear model using the robust linear regression.
}
\usage{
lm.coefs(x, y, method.reg)
}
%- maybe also 'usage' for other objects documented here.
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
\code{lm.coefs} is convinient wrapper around few functions performing normal and robust
linear regression. If the robut linear regression is not possible, \code{lm.coefs} will 
perform linear regression using the least squares method.
}
\value{
Data frame with one column and two rows representing coefficients of the linear
model.
}

\author{
Stefan Roediger, Michal Burdukiewicz
}


\seealso{
\code{\link[quantreg]{rq}}, \code{\link[Rfit]{rfit}}, 
\code{\link[stats]{lm}}
}
\examples{
lm.coefs(VIMCFX96_69[1:10, 1], VIMCFX96_69[1:10, 2], method.reg = "rq")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ robust }
\keyword{ regression }