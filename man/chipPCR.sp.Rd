\name{chipPCR.sp}
\alias{chipPCR.sp}
\docType{data}
\title{
Scientific plots of the \code{chipPCR} package
}
\description{
This Rd-file contains sophisticated plots and example calculations for the 
\code{chipPCR} package. Selected plots and calculations require additional 
packages and functions (e.g., \code{\link[drc]{drm}}).
}
\source{
Stefan Roediger, Michal Burdukiewicz
}

\examples{
\dontrun{
###########################################################################
# Figure 1
#################
pdf(file = "problems.pdf", width = 12, height = 8)
# Use AmpSim to generate an amplification curve with 40 cycles
# and a different Cq
res.pos <- AmpSim(cyc = 1:40, noise = TRUE, nnl = 0.05)
res.pos[5, 2] <- res.pos[5, 2] * 6

res.low <- AmpSim(cyc = 1:40, noise = TRUE, bl = 0.5, ampl = 0.58, Cq = 33)
res.low[c(31), 2] <- NA

res.neg <- AmpSim(cyc = 1:40, b.eff = -0.1, bl = 0.05, ampl = 0.4, Cq = 1, 
		    noise = FALSE, nnl = 0.5)
		      
res.pos.CPP <- cbind(1:40, CPP(res.pos[, 1], res.pos[, 2], 
		      bg.outliers = TRUE, smoother = TRUE, method = 
"smooth", 
		      norm = "luqn", rob.reg = "lmrob")$y)
		      
res.low.NA <- cbind(1:40, CPP(res.low[, 1], res.low[, 2], smoother = TRUE, 
		    method = "smooth", bg.outliers = TRUE, norm = "luqn", 
		    rob.reg = "lmrob")$y)
		      
res.neg.exc <- cbind(1:40, amptester(res.neg[, 2]))

par(mfrow = c(1,2), las = 0, bty = "n", cex.axis = 1.5, cex.lab = 1.5, 
    font = 2, cex.main = 1.8, oma = c(1,1,1,1))
plot(NA, NA, xlim = c(1,40), ylim = c(0, 1.5), xlab = "Cycle", 
     ylab = "Raw fluorescence")
mtext("A", cex = 2, side = 3, adj = 0, font = 2)
# abline(lm(res.pos[1:10, 2] ~  res.pos[1:10, 1]))
lines(res.pos)
lines(res.low, col = 2)
arrows(38, min(res.low[, 2], na.rm = TRUE), 38, max(res.low[, 2], na.rm = 
TRUE), 
	code=3, lwd=3, angle=90, col="grey")
text(38, max(res.low[, 2], na.rm = TRUE) * 0.7,"SNR", cex=1.2)

arrows(29,0.42,31,0.51, lwd=2)
text(29,0.38, "NA", cex=1.2)

points(res.pos[5, 1], res.pos[5, 2], pch=21, cex=4, lwd=5, col="orange")
text(res.pos[5, 1], res.pos[5, 2] * 1.2, "Outlier", cex=1.2)

lines(res.neg, col = 4)
text(20, mean(res.neg[, 2]) *0.9, "No amplification", cex=1.2, col = 
"blue")


plot(NA, NA, xlim = c(1,40), ylim = c(0, 1.5), xlab = "Cycle", 
     ylab = "Pre-processed fluorescence")
mtext("B", cex = 2, side = 3, adj = 0, font = 2)

lines(res.pos.CPP)
lines(res.low.NA, col = 2)
lines(res.neg.exc, col = 4)

dev.off()

###########################################################################
# Figure 2
#################
# Load the shiny package (chipPCR should already be loaded).
# Run from a R console following commands.
require(shiny)

# Invoke the shiny AmpSim app in the default browser.
runApp(paste0(find.package("chipPCR")[1],"/AmpSim.gui"))

###########################################################################
# Figure 3
#################
pdf(file = "AmpSim_random.pdf", width = 12, height = 6)
# Draw an empty plot for 50 cycles
par(las = 0, bty = "n", cex.axis = 1.2, cex.lab = 1.2, 
    font = 2, cex.main = 1.2, oma = c(1,1,1,1))
plot(NA, NA, xlim = c(1,50), ylim = c(0,1.1), xlab = "Cycle", 
     ylab = "RFU")
colors <- rainbow(8)
# Create a loop for 8 amplification curves 
# The approximate Cqs are synthesize the temporary Cqs
# by adding a random value to a starting Cq of 25
# Note: ``noise'' is set TRUE with a level of 0.03. This 
# adds some noise to the plot.
for (i in 1:8) {
  Cq.tmp <- 25 + rnorm(1) * 5
  
  tmp <- AmpSim(1:50, Cq = Cq.tmp, noise = TRUE, nnl = 0.03)
  lines(tmp, col = colors[i])
  
  # Add the approximate Cq values to the plot
  text(1, 1 - i / 10, paste("Cq ", round(Cq.tmp, 2)), col = colors[i])
}
dev.off()


###########################################################################
# Figure 4
#################
pdf(file = "SDM.pdf", width = 12, height = 8)
# Use AmpSim to generate an amplification curve with 40 cycles
# and an approximate Cq of 20 and assign it to the object isPCR.
# isPCR will an object of the class "data.frame".
isPCR <- AmpSim(cyc = 1:40, Cq = 20)

# Invoke the inder function for the object isPCR to interpolate 
# the derivatives of the simulated data as object res. The Nip 
# parameter was set to 5. This leads to smoother curves. res is
# an object of the class "der".
res <- inder(isPCR, smooth.method = "spline", Nip = 5)

# Plot the the object res and add description to the elements.
par(las = 0, bty = "n", cex.axis = 1.5, cex.lab = 1.5, 
    font = 2, cex.main = 1.8, oma = c(1,1,1,1))

plot(isPCR, xlab = "Cycle", ylab = "RFU", ylim = c(-0.15,1),
     main = "", type = "b", pch = 20)

# Add graphical elements for the dervatives and the calculated
# Cq values FDM, SDM, SDm and SDC
  lines(res[, "x"], res[, "d1y"], col = "blue")
  lines(res[, "x"], res[, "d2y"], col = "red")
  summ <- summary(res, print = FALSE)
  abline(v = c(summ["SDM"], summ["SDm"], summ["SDC"]), col = c(3,4,5))
  text(summ["SDM"], 0.5, paste("SDM ~ ", round(summ["SDM"], 2)), 
       cex = 1.5, col = 3)
  text(summ["SDC"], 0.7, paste("SDC ~ ", round(summ["SDC"], 2)), 
       cex = 1.5, col = 5)
  text(summ["SDm"], 0.9, paste("SDm ~ ", round(summ["SDm"], 2)), 
       cex = 1.5, col = 4)
  text(summ["FDM"] + 10, 0.65, paste("FDM ~ ", round(summ["FDM"], 2)), 
       cex = 1.5, col = 1)
       
  legend(1.1, 0.9, c("raw", "first derivative", "second derivative"), 
         col = c(1,4,2), lty = c(2,1,1), cex = 1.2, bty = "n")

# Summary of the object res.
summ
#     FDM      SDM      SDm      SDC 
#19.81407 19.03015 20.98995 19.98604 
dev.off()
###########################################################################

  }
}
\keyword{ CPP }
\keyword{ amptester }
\keyword{ smoother }
\keyword{ qPCR }
\keyword{ qIA }
\keyword{ isothermal }
\keyword{ real-time }
\keyword{ VideoScan }
\keyword{ ccPCR }
\keyword{ Bio-Rad }
\keyword{ iQ5 }
\keyword{ CFX96 }
