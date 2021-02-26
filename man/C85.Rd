\name{C85}
\alias{C85}
\docType{data}
\title{
Helicase Dependent Amplification of Vimentin using the 'VideoScan' Platform
}
\description{
A Helicase Dependent Amplification (HDA) of Vimentin (Vim) was performed. The 
'VideoScan' Platform (Roediger et al. (2013)) was used to monitor the 
amplification. The HDA was performed at 65 degrees Celsius. Three 
concentrations of input DNA (D1, D2, D3) were used.
}
\usage{data(C85)}
\format{
  A data frame with 301 observations on the following 5 variables.
  \describe{
    \item{\code{Cycle}}{Cycles HDA measurements.}
    \item{\code{t.D1}}{Dilution 1, elapsed time during HDA in seconds.}
    \item{\code{MFI.D1}}{Dilution 1, fluorescence.}
    \item{\code{t.D2}}{Dilution 2, elapsed time during HDA in seconds.}
    \item{\code{MFI.D2}}{Dilution 2, fluorescence.}
    \item{\code{t.D3}}{Dilution 3, elapsed time during HDA in seconds.}
    \item{\code{MFI.D3}}{Dilution 3, fluorescence.}
  }
}
\details{

To perform an isothermal amplification in 'VideoScan', standard conditions for 
the IsoAmp(R) III Universal tHDA Kit (Biohelix) were used. Primers and 
templates are described in Roediger et al. (2013). The reaction was composed 
of reaction mix A)10 micro L A. bidest, 1.25 micro L 10xbuffer, 0.75 micro L 
primer(150nM final), 0.5 micro L template plasmid. Preincubation: This mixture 
was incubated for 2 min at 95 degree. Celsius and immediately placed on ice. 
Reaction mix B) 5 micro L A. bidest., 1.25 micro L 10x buffer, 2 micro L NaCl, 
1.25 micro L MgSO4, 1.75 micro L dNTPs, 0.25 micro L EvaGreen, 1 micro L 
enzyme mix. The mix was covered with 50 micro L mineral oil. The fluorescence 
measurement in 'VideoScan' 'HCU' started directly after adding buffer B at 65 
degrees Celsius. A 1x (D1), a 1:10 dilution (D2) and a 1:100 (D3) dilution were 
tested.
Temperature profile (after Preincubation):
- 60 seconds at 65 degrees Celsius 
- 11 seconds at 55 degrees Celsius && Measurement
}
\source{
Claudia Deutschmann & Stefan Roediger, BTU Cottbus - Senftenberg, Senftenberg, 
Germany
}
\references{
A Highly Versatile Microscope Imaging Technology Platform for the Multiplex 
Real-Time Detection of Biomolecules and Autoimmune Antibodies. S. Roediger, 
P. Schierack, A. Boehm, J. Nitschke, I. Berger, U. Froemmel, C. Schmidt, 
M. Ruhland, I. Schimke, D. Roggenbuck, W. Lehmann and C. Schroeder.
\emph{Advances in Biochemical Bioengineering/Biotechnology}. 133:33--74, 2013.

}
\examples{
data(C85)
# First example
plot(NA, NA, xlim = c(0,85), ylim = c(0,1), xlab = "Time [min]", 
      ylab = "Fluorescence", main = "HDA amplification")
points(C85[, 2]/60, C85[, 3], type = "b", col = 1, pch = 20)
points(C85[, 4]/60, C85[, 5], type = "b", col = 2, pch = 20)
points(C85[, 6]/60, C85[, 7], type = "b", col = 3, pch = 20)
legend(40, 0.5, c("D1, 1x", "D2, 1:10", "D3, 1:100"), col = c(1:3), 
	pch = rep(20,3))

# Second example
plot(NA, NA, xlim = c(0,30), ylim = c(0,0.8), xlab = "Time [min]", 
      ylab = "Fluorescence", main = "HDA amplification")
points(C85[, 2]/60, C85[, 3], type = "b", col = 1, pch = 20)
points(C85[, 2]/60, smoother(C85[, 2]/60, C85[, 3], 
      method = list("savgol")), type = "b", col = 2, pch = 20)
points(C85[, 2]/60, smoother(C85[, 2]/60, C85[, 3], 
      method = list("smooth")), type = "b", col = 3, pch = 20)
points(C85[, 2]/60, smoother(C85[, 2]/60, C85[, 3], 
      method = list("mova")), type = "b", col = 4, pch = 20)

legend(1, 0.8, c("D1, raw", "D1, savgol", "D1, smooth", "D1, mova"), 
	col = c(1:4), pch = rep(20,4))

# Third example
# Comparison of Lowess, Moving average and splines to smooth amplification 
# curve data of
# a HDA using the 'VideoScan' 'HCU' for amplification and monitoring.

xrange <- 2:2400
plot(NA, NA, xlim = c(0,85), ylim = c(0.4, 0.8), xlab = "Time [min]", 
      ylab = "RFI", main = "Raw data")
points(C85[, 2]/60, C85[, 3], type = "b", col = 1, pch = 20)
points(C85[, 4]/60, C85[, 5], type = "b", col = 2, pch = 20)
points(C85[, 6]/60, C85[, 7], type = "b", col = 3, pch = 20)
legend("topleft", c("D1, 1x", "D2, 1:10", "D3, 1:100"), col = c(1:3), 
	pch = rep(20,3))

mtext("A", cex = 2, side = 3, adj = 0, font = 2)

plot(NA, NA, xlim = c(0,40), ylim = c(-0.05, 0.3), xlab = "Time [min]", 
      ylab = "RFI", main = "Moving average")
movaww <- seq(1,17,4)
for (i in 1:length(movaww)) {
  for (j in c(2,4,6)) {
    tmp <- data.frame(na.omit(C85[xrange, j])/60, na.omit(C85[xrange, j + 1]))
    tmp.out <- smoother(tmp[, 1], tmp[, 2], method = list(mova = list(movaww = movaww[i])), 
			bg.outliers = TRUE)
    lines(data.frame(tmp[, 1], tmp.out), type = "l", pch = 20, cex = 0.5, 
	  col = i)
    }
}
mtext("B", cex = 2, side = 3, adj = 0, font = 2)
legend("topleft", c("D1, 1x", "D2, 1:10", "D3, 1:100"), col = c(1:3), 
	pch = rep(20,3))
legend("bottomright", 6, paste("movaww : ", movaww), pch = 20, lwd = 2, 
	col = 1:length(movaww))
    
plot(NA, NA, xlim = c(0,40), ylim = c(-0.05, 0.3), xlab = "Time [min]", 
      ylab = "RFI", main = "Cubic Spline")
df.fact <- seq(0.5,0.9,0.1)
for (i in 1:length(df.fact)) {
  for (j in c(2,4,6)) {
    tmp <- data.frame(na.omit(C85[xrange, j])/60, na.omit(C85[xrange, j + 1]))
    tmp.out <- smoother(tmp[, 1], tmp[, 2], 
		  method = list(smooth = list(df.fact = df.fact[i])), 
		  bg.outliers = TRUE)
    
    lines(data.frame(tmp[, 1], tmp.out), type = "l", pch = 20, 
    cex = 0.5, col = i)
    }
}
    
mtext("C", cex = 2, side = 3, adj = 0, font = 2)
legend("topleft", c("D1, 1x", "D2, 1:10", "D3, 1:100"), col = c(1:3), 
pch = rep(20,3))
legend("bottomright", paste("df.fact : ", df.fact), pch = 20, lwd = 2, 
col = 1:length(df.fact))
    
plot(NA, NA, xlim = c(0,40), ylim = c(-0.05, 0.3), xlab = "Time [min]", 
ylab = "RFI", main = "Lowess")
f <- seq(0.01,0.2,0.04)
for (i in 1:length(f)) {
  for (j in c(2,4,6)) {
    tmp <- data.frame(na.omit(C85[xrange, j])/60, na.omit(C85[xrange, j + 1]))
    tmp.out <- smoother(tmp[, 1], tmp[, 2], method = list(lowess = list(f = f[i])), 
		  bg.outliers = TRUE)
    
    lines(data.frame(tmp[, 1], tmp.out), type = "l", pch = 20, cex = 0.5, 
    col = i)
    }
    }
    
    mtext("D", cex = 2, side = 3, adj = 0, font = 2)
    legend("topleft", c("D1, 1x", "D2, 1:10", "D3, 1:100"), col = c(1:3), 
    pch = rep(20,3))
    legend("bottomright", paste("f : ", f), pch = 20, lwd = 2, col = 1:length(f))
	
plot(NA, NA, xlim = c(0,40), ylim = c(-0.05, 0.3), xlab = "Time [min]", 
ylab = "RFI", main = "Friedman's\n''super smoother''")
span <- seq(0.01,0.05,0.01)
for (i in 1:length(span)) {
  for (j in c(2,4,6)) {
    tmp <- data.frame(na.omit(C85[xrange, j])/60, na.omit(C85[xrange, j + 1]))
    tmp.out <- smoother(tmp[, 1], tmp[, 2], method = list(supsmu = list(span = span[i])), 
	  bg.outliers = TRUE)
    
    lines(data.frame(tmp[, 1], tmp.out), type = "l", pch = 20, cex = 0.5, 
col = i)
    }
    }
    
    mtext("E", cex = 2, side = 3, adj = 0, font = 2)
    legend("topleft", c("D1, 1x", "D2, 1:10", "D3, 1:100"), col = c(1:3), 
pch = rep(20,3))
    legend("bottomright", paste("span : ", span), pch = 20, lwd = 2, col = 1:length(span))
	  
plot(NA, NA, xlim = c(0,40), ylim = c(-0.05, 0.3), xlab = "Time [min]", 
ylab = "RFI", main = "Savitzky-Golay")

for (j in c(2,4,6)) {
  tmp <- data.frame(na.omit(C85[xrange, j])/60, na.omit(C85[xrange, j + 1]))
  tmp.out <- smoother(tmp[, 1], tmp[, 2], method = list("savgol"), 
	bg.outliers = TRUE)
  
  lines(data.frame(tmp[, 1], tmp.out), type = "l", pch = 20, cex = 0.5, 
col = 1)
  }
  
  mtext("F", cex = 2, side = 3, adj = 0, font = 2)
  legend("bottomright", paste("/ : ", NULL), pch = 20, lwd = 2, col = 1:length(span))
}
\keyword{ datasets }
\keyword{ HDA }
\keyword{ EvaGreen }
\keyword{ HCU }
\keyword{ VIM }
\keyword{ VideoScan }
