\name{chipPCR.datasets}
\alias{chipPCR.datasets}
\docType{data}
\title{
Overview for data sets of the \code{chipPCR} package
}
\description{
The \code{chipPCR} package contains numerous data sets from commercial and 
experimental technologies for the amplification of nucleic acids. The data 
sets include results from qPCR experiments with the 'VideoScan' 'HCU' 
(Roediger et al. 2013), Bio-Rad (iQ5, CFX96), capillary convective PCR (ccPCR) and 
Roche Light Cycler 1.5. Real-time monitored amplification reactions were 
performed using standard amplification methods (qPCR, based on Taq 
polymerase) and quantitative isothermal amplification (qIA). In selected 
data 
sets are melting curves and dilution series available. Most of the data 
sets have equidistant measure points. However, some datasets have none 
homogeneous measure points as indicated below.

\strong{capillary convective PCR (ccPCR)}

\code{\link[chipPCR]{capillaryPCR}}:
The capillary convective PCR (ccPCR) is a modified device of the ccPCR 
system proposed by Chou et al. 2013.

\strong{standard qPCR - commercial thermo cyclers}

\code{\link[chipPCR]{C60.amp}}:
qPCR Experiment for the Amplification of MLC-2v and Vimentin (as decadic 
dilutions) Using the Roche Light Cycler 1.5.

\code{\link[chipPCR]{C60.melt}}:
Melt Curves MLC-2v and Vimentin for the qPCR experiment 
\code{\link[chipPCR]{C60.amp}} using the Roche Light Cycler 1.5

\code{\link[chipPCR]{C126EG595}}:
A quantitative PCR (qPCR) with the DNA binding dye (EvaGreen) (Mao et al. 
2007) was performed in a Bio-Rad iQ5 thermo cycler. The 
cycle-dependent increase of the fluorescence was quantified at the 
elongation step (59.5 degrees Celsius).

\code{\link[chipPCR]{C126EG685}}:
A quantitative PCR (qPCR) with the DNA binding dye (EvaGreen) (Mao et al. 
2007) was performed in a Bio-Rad iQ5 thermo cycler. The 
cycle-dependent increase of the fluorescence was quantified at the 
elongation step (68.5 degrees Celsius).

\code{\link[chipPCR]{C127EGHP}}:
Quantitative PCR (qPCR) with a hydrolysis probe (Cy5/BHQ2) and DNA binding 
dye (EvaGreen) (Mao et al. 2007) performed in the Roche Light Cycler 
1.5 thermo cycler.

\code{\link[chipPCR]{VIMCFX96_60}}:
Human vimentin amplification curve data (measured during annealing phase at 
60 degrees Celsius) for 96 replicate samples in a Bio-Rad CFX96 thermo cycler.

\code{\link[chipPCR]{VIMCFX96_69}}:
Human vimentin amplification curve data (measured during elongation phase at 
69 degrees Celsius) for 96 replicate samples in a Bio-Rad CFX96 thermo cycler.

\code{\link[chipPCR]{VIMCFX96_meltcurve}}:
Human vimentin melting curve data for 96 replicate samples in a Bio-Rad 
CFX96 thermo cycler.

\code{\link[chipPCR]{VIMiQ5_595}}:
Human vimentin amplification curve data (measured during annealing phase at 
59.5 degrees Celsius) for 96 replicate samples in a Bio-Rad iQ5 thermo cycler.

\code{\link[chipPCR]{VIMiQ5_685}}:
Human vimentin amplification curve data (measured during elongation phase at 
68.5 degrees Celsius) for 96 replicate samples in a Bio-Rad iQ5 thermo cycler.

\code{\link[chipPCR]{VIMiQ5_melt}}:
Human vimentin melting curve data for 96 replicate samples in a Bio-Rad 
iQ5 thermo cycler.

\strong{standard qPCR - experimental thermo cyclers}

\code{\link[chipPCR]{C54}}:
qPCR Experiment in the 'VideoScan' heating/cooling-unit for the 
amplification using different concentrations of MLC-2v input cDNA 
quantities. 

\code{\link[chipPCR]{CD74}}:
Quantitative PCR with a hydrolysis probe and DNA binding dye (EvaGreen) for 
MLC-2v measured at 59.5 degrees Celsius (annealing temperature), 68.5 
degrees Celsius (elongation temperature) and at 30 degrees Celsius.

\strong{Simulations}

\code{\link[chipPCR]{Eff625}}:
Highly replicate number amplification curves with an approximate 
amplification efficiency of 62.5 percent at cycle number 18. The data were 
derived from a simulation such as the AmpSim function.

\code{\link[chipPCR]{Eff750}}:
Highly replicate number amplification curves with an approximate 
amplification efficiency of 75 percent at cycle number 18. The data were 
derived from a simulation such as the AmpSim function.

\code{\link[chipPCR]{Eff875}}:
Highly replicate number amplification curves with an approximate 
amplification efficiency of 87.5 percent at cycle number 18. The data were 
derived from a simulation such as the AmpSim function.

\code{\link[chipPCR]{Eff1000}}:
Highly replicate number amplification curves with an approximate 
amplification efficiency of 100 percent at cycle number 18. The data were 
derived from a simulation such as the AmpSim function.

\strong{Isothermal Amplification - Helicase Dependent Amplification}

\code{\link[chipPCR]{C67}}:
A Helicase Dependent Amplification (HDA) of HPRT1 (Homo sapiens 
hypoxanthine phosphoribosyltransferase 1), performed at different 
input DNA quantities using the Bio-Rad iQ5 thermo cycler.

\code{\link[chipPCR]{CD75}}:
Helicase Dependent Amplification in the 'VideoScan' 'HCU' of HPRT1 (Homo 
sapiens hypoxanthine phosphoribosyltransferase 1) measured at at 55, 60 or 
65 degrees Celsius.

\code{\link[chipPCR]{C81}}:
Helicase Dependent Amplification (HDA) of pCNG1 using the 'VideoScan' 
Platform (Roediger et al. (2013)). The HDA was performed at 65 degree 
Celsius. Two concentrations of input DNA were used.

\code{\link[chipPCR]{C85}}:
Helicase Dependent Amplification (HDA) of Vimentin (Vim) in the 'VideoScan' 
Platform (Roediger et al. (2013)). The HDA was performed at 65 degree 
Celsius with three dilutions of input DNA.

}

\source{
Stefan Roediger
}
\references{
A Highly Versatile Microscope Imaging Technology Platform for the 
Multiplex 
Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. 
Roediger, 
P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, 
M. 
Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder. 
\emph{Advances in Biochemical Bioengineering/Biotechnology}. 133:33--74, 
2013. 
}
\examples{
data(VIMiQ5_melt)

tmp <- VIMiQ5_melt

plot(NA, NA, xlim = c(55,95), ylim = c(0, 40000), 
    xlab = "Temperature (degrees Celsius)",ylab = "RFU", 
    main = "Melting curve in iQ5 (Bio-Rad)")
apply(tmp[, 2:ncol(tmp)], 2, 
      function(x) lines(tmp[1:nrow(tmp),1],x))

Fmean <- rowMeans(tmp[, 2:ncol(tmp)])
lines(tmp[1:nrow(tmp),1], Fmean, col = "red", lwd = 3)

legend(55, 4000, c("Raw", "Mean"), pch = c(19,19), col = c(1,2))
}
\keyword{ datasets }
\keyword{ melt }
\keyword{ iQ5 }
\keyword{ qPCR }
\keyword{ qIA }
\keyword{ isothermal }
\keyword{ real-time }
\keyword{ VideoScan }
\keyword{ ccPCR }
\keyword{ Bio-Rad }
\keyword{ iQ5 }
\keyword{ CFX96 }
