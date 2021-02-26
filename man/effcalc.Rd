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
 effcalc calculates the amplification efficiency of a dilution.
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
  
  \item{y}{ are multiple columns of Cq values from a 
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
  an object of the class \code{\linkS4class{eff}}.
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
2013. 

S. Mehra and W.-S. Hu. A kinetic model of quantitative real-time polymerase 
chain reaction. \emph{Biotechnology and bioengineering}, 91(7):848--860, 
Sept. 2005. ISSN 0006-3592. doi: 10.1002/bit.20555. PMID: 15986490.

M. Guescini, D. Sisti, M. B. Rocchi, L. Stocchi, and V. Stocchi. A new 
real-time PCR method to overcome significant quantitative inaccuracy due to 
slight amplification inhibition. \emph{BMC Bioinformatics}, 9(1):326, July 
2008. ISSN 1471-2105. doi: 10.1186/1471-2105-9-326. PMID: 18667053.

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

\keyword{ efficiency }
\keyword{ methods }
