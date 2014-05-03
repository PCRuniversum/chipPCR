\name{effcalc}
\alias{effcalc}

\title{
Analysis of the emplification efficiency
}
\description{
 effcalc is used for a fast calculation of the amplification efficiency of
 a dilution. \code{effcalc} returns an object of the class 
 list.
}
\usage{
effcalc(x, y, logx = TRUE, CV = FALSE, 
		    xlab = "log10(Concentration)", ylab = "Cq", 
		    main = "Efficiency Plot", RSD = FALSE, rob = FALSE, 
		    trend = TRUE, res.fit = TRUE, CI = FALSE, level = 0.95,
		    show.res = TRUE, type = "p", pch = 19, length = 0.05, 
		    col = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ is the column of a data frame for the concentration 
	    (dilution). }
  
  \item{y}{ are multiple columns of fluorescence values from a 
  
  \code{data.frame} (e.g., [, c(1:n)]). }
  
  \item{logx}{ is a logical parameter used to convert the concentration 
	      into a decadic logarithm. }
  
  \item{CV}{ If \code{CV} is true the coefficient of variation (RSD, CV) is 
  plotted. If set to FLASE the deviation as Standard Deviation or Median 
  Absolute Deviation is plotted. }

  \item{xlab}{ a title for the x axis. }
  
  \item{ylab}{ a title for the y axis. }
  
  \item{main}{ an overall title for the plot. }
  
  \item{RSD}{ Setting the option \code{RSD = TRUE} shows the relative 
  standard deviation (RSD) in percent. }
  
  \item{rob}{ Using the option \code{rob} as TRUE the median and the median 
  absolute deviation (MAD) is plotted instead of the mean and standard 
  deviation. }

  \item{trend}{ Setting the option \code{trend = TRUE} shows the linear 
   regression line in the plot. }
  
  \item{res.fit}{ Setting the option \code{res.fit = TRUE} shows the 
    results (goodness of fit, amplification efficiency, correlation ) 
    of the linear regression line in the plot. }
    
  \item{CI}{ Setting the option \code{CI = TRUE} shows the confidence
   interval lines in the plot. }

  \item{level}{ Tolerance/confidence level. }

  \item{show.res}{ Setting the option \code{res.fit = TRUE} shows the 
  standard output. }

  \item{type}{ is a graphical parameter setting the plot use lines, points 
  or both (see \code{\link{plot}}). }
  
  \item{pch}{ is a graphical parameter used to define the symbol used in 
  the plot. }
  
  \item{length}{ \code{length} is a graphical parameter used to define the 
  length of the error bar used in the plot. }

  \item{col}{ \code{col} is a graphical parameter used to define the length 
  of the error bar used in the plot. }
  
}

\value{
  \item{res}{
      returns a \code{list} containing the "Concentration", "Location" 
      (mean, median), "Deviation" (standard deviation, median absolute 
      deviation), "Coefficient of Variance" (CV, RSD) sequential in the 
      columns, the results of the amplification efficiency calculation,
      the results of the linear regression and the correlation test 
      (Pearson) results.}
}

\author{
Stefan Roediger
}

\references{
A Highly Versatile Microscope Imaging Technology Platform for the Multiplex 
Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. Roediger, 
P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, M. 
Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder. 
\emph{Advances in Biochemical Bioengineering/Biotechnology}. 133:33--74, 
2013. \url{http://www.ncbi.nlm.nih.gov/pubmed/22437246}

Nucleic acid detection based on the use of microbeads: a review. S. 
Roediger, C. Liebsch, C. Schmidt, W. Lehmann, U. Resch-Genger, U. Schedler, 
P. Schierack. \emph{Microchim Acta} 2014:1--18. DOI: 
10.1007/s00604-014-1243-4
}
\examples{
# First Example
# Amplification efficiency plot 

# require(MBmca)
# par(mfrow = c(1,2), las = 0, bty = "n", cex.axis = 1.2, cex.lab = 1.2, 
#    font = 2, cex.main = 1, oma = c(0.1, 0.1, 0.5, 0.1))

# Simulate a qPCR reaction with AmpSim for 45 cycles and some noise.
# plot(NA, NA, xlim = c(1,45), ylim = c(0.01,1.1), xlab = "Cycles", 
#      ylab = "Fluorescence", 
#      main = "")
# mtext("A", cex = 2, side = 3, adj = 0, font = 2)
# cycle <- rep(seq(15, 34, 3.5), 3)

# dilution <-rep(c(100, 10, 1, 0.1, 0.01, 0.001), 3)

# ma.out <- matrix(data = NA, nrow = 45, 
# 		 ncol = length(cycle))
		 
# Cq.out <- vector()

# for (i in 1:18) {
#   ma.out[1:45, i] <- AmpSim(cyc = c(1:45), b.eff = -50, bl = 0.001, 
# 			      ampl = 1, Cq = cycle[i], noise = TRUE, 
# 			      nnl = 0.02)[, 2]
#   lines(1:45, ma.out[, i])
#   tmpP <- mcaSmoother(1:45, ma.out[, i])
#   Cq.tmp <- diffQ2(tmpP, inder = TRUE)$xTm1.2.D2[1]
#   abline(v = Cq.tmp)
#   Cq.out <- c(Cq.out, Cq.tmp)
# }

# tmp <- data.frame(dilution[1:6],
# 		  Cq.out[1:6],
# 		  Cq.out[7:12],
# 		  Cq.out[13:18])

# effcalc(tmp[, 1], tmp[, 2:4], CI = TRUE)
# mtext("B", cex = 2, side = 3, adj = 0, font = 2) 

}

\keyword{ efficiency }
