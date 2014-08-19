\name{effcalc}
\alias{effcalc}
\alias{effcalc.numeric}
\alias{effcalc.matrix}
\alias{effcalc.data.frame}
\alias{effcalc,numeric,numeric-method}
\alias{effcalc,matrix,missing-method}
\alias{effcalc,data.frame,missing-method}

\title{
Analysis of the amplification efficiency
}
\description{
 effcalc is used for a fast calculation of the amplification efficiency of
 a dilution. \code{effcalc} returns an object of the class 
 list.
}
\usage{
\S4method{effcalc}{numeric,numeric}(x, y, logx = TRUE, RSD = FALSE, rob = FALSE,
                                    level = 0.95)
\S4method{effcalc}{matrix,missing}(x, y, logx = TRUE, RSD = FALSE, rob = FALSE,
                                   level = 0.95)
\S4method{effcalc}{matrix,missing}(x, y, logx = TRUE, RSD = FALSE, rob = FALSE,
                                   level = 0.95)        
}

\arguments{
  \item{x}{ is the column of a data frame for the concentration 
	    (dilution). }
  
  \item{y}{ are multiple columns of fluorescence values from a 
  \code{data.frame} (e.g., [, c(1:n)]). }
  
  \item{logx}{ is a logical parameter used to convert the concentration 
	      into a decadic logarithm. }
  
  \item{RSD}{ Setting the option \code{RSD = TRUE} shows the relative 
  standard deviation (RSD) in percent. }
  
  \item{rob}{ Using the option \code{rob} as TRUE the median and the median 
  absolute deviation (MAD) is plotted instead of the mean and standard 
  deviation. }
  
  \item{level}{ Tolerance/confidence level. }
  
}

\value{
  \code{\linkS4class{eff}} object.
}

\author{
Stefan Roediger
}

\seealso{ 
\code{\linkS4class{eff}}

\code{\link{plot.eff}}
}

\references{
A Highly Versatile Microscope Imaging Technology Platform for the Multiplex 
Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. Roediger, 
P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, M. 
Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder. 
\emph{Advances in Biochemical Bioengineering/Biotechnology}. 133:33--74, 
2013. \url{http://www.ncbi.nlm.nih.gov/pubmed/22437246}

S. Mehra and W.-S. Hu. A kinetic model of quantitative real-time polymerase 
chain reaction. \emph{Biotechnology and bioengineering}, 91(7):848--860, 
Sept. 2005. ISSN 0006-3592. doi: 10.1002/bit.20555. PMID: 15986490.

M. Guescini, D. Sisti, M. B. Rocchi, L. Stocchi, and V. Stocchi. A new 
real-time PCR method to overcome significant quantitative inaccuracy due to 
slight amplification inhibition. \emph{BMC Bioinformatics}, 9(1):326, July 
2008. ISSN 1471-2105. doi: 10.1186/1471-2105-9-326. \url{http: 
//www.biomedcentral.com/1471-2105/9/326/abstract}. PMID: 18667053.

A. Tichopad, M. Dilger, G. Schwarz, and M. W. Pfaffl. Standardized 
determination of real-time PCR efficiency from a single reaction set-up. 
\emph{Nucleic Acids Research}, 31(20):e122, Oct. 2003. ISSN 1362-4962. 
PMID: 14530455 PMCID: PMC219490.

A. Staalberg, P. Aman, B. Ridell, P. Mostad, and M. Kubista. Quantitative 
real-time PCR method for detection of b-lymphocyte monoclonality by 
comparison of kappa and lambda immunoglobulin light chain expression. 
\emph{Clinical Chemistry}, 49(1):51--59, Jan. 2003. ISSN 0009-9147. PMID: 
12507960.

W. Liu and D. A. Saint. A new quantitative method of real time reverse 
transcription polymerase chain reaction assay based on simulation of 
polymerase chain reaction kinetics. \emph{Analytical Biochemistry}, 
302(1):52--59, Mar. 2002. ISSN 0003-2697. doi: 10.1006/abio.2001.5530. 
PMID: 11846375.

}
\examples{
## Not run: 
# First Example
# Amplification efficiency plot 

require(MBmca)
par(mfrow = c(1,2), las = 0, bty = "n", cex.axis = 1.2, cex.lab = 1.2, 
    font = 2, cex.main = 1, oma = c(0.1, 0.1, 0.5, 0.1))

# Simulate a qPCR reaction with AmpSim for 45 cycles and some noise.
# Define number of cycles
cyc.no <- 45

# Create an empty plot
plot(NA, NA, xlim = c(1,cyc.no), ylim = c(0.01,1.1), xlab = "Cycles", 
     ylab = "Fluorescence", main = "")
mtext("A", cex = 2, side = 3, adj = 0, font = 2)

# Create a sequence of "whised" Cq value for the simulation
cycle <- rep(seq(15, 34, 3.5), 3)

# Define the levels for the decadic dilution with concentrations
# from 100 to 0.001 (six steps).
# The in-slico experiment is designed to have three replicates at
# six dilution steps.
dilution <-rep(c(100, 10, 1, 0.1, 0.01, 0.001), 3)

# Create an empty matrix for the results of the concentration
# dependent Cq values
ma.out <- matrix(data = NA, nrow = cyc.no, 
		 ncol = length(cycle))

# Use AmpSim to simulate amplification curves at different 
# concentrations. The simulation is performed with the addition
# of some noise. This will do a generation of unique amplification
# curves, even under idential paramter settings. Calculate the 
# pseudo Second Derivative Maximum (SDM) (Cq) by using the the 
# diffQ2 function from the MBmca package.

Cq.out <- vector()

for (i in 1:18) {
  ma.out[1:cyc.no, i] <- AmpSim(cyc = c(1:cyc.no), b.eff = -50, bl = 0.001, 
				ampl = 1, Cq = cycle[i], noise = TRUE, 
				nnl = 0.02)[, 2]
  lines(1:cyc.no, ma.out[, i])
  tmpP <- mcaSmoother(1:cyc.no, ma.out[, i])
  #TURNED OFF INDER - NEW COMPATIBILITY TO MBmca STILL PENDING
  Cq.tmp <- diffQ2(tmpP, inder = FALSE)$xTm1.2.D2[1]
  abline(v = Cq.tmp)
  Cq.out <- c(Cq.out, Cq.tmp)
}

# Assign the calculated Cqs to the corresponding concentrations
tmp <- data.frame(dilution[1:6],
		  Cq.out[1:6],
		  Cq.out[7:12],
		  Cq.out[13:18])
		  
# Determine the amplification efficiency by using the effcalc function
plot(effcalc(tmp[, 1], tmp[, 2:4]), CI = TRUE)
mtext("B", cex = 2, side = 3, adj = 0, font = 2) 
par(mfrow = c(1,1))
## End(Not run)
}

\keyword{ efficiency }
\keyword{ methods }