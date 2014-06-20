setGeneric("summary")
setGeneric("plot")

#just for writing comfort, self explanatory
setClassUnion("numericOrNULL",c("numeric","NULL"))

#amptest class, amptester function -------------------------------
setClass("amptest", contains = "numeric", representation(.Data = "numeric", 
                                                         decisions = "logical",
                                                         noiselevel = "numeric",
                                                         background = "numericOrNULL"))

setMethod("show", signature(object = "amptest"), function(object) {
  print(slot(object, ".Data"))
})

setMethod("summary", signature(object = "amptest"), function(object) {
  print(slot(object, ".Data"))
  cat(paste0("\nAmplification significance (threshold test): ", slot(object, "decisions")[["tht.dec"]]))
  cat(paste0("\nAmplification significance (signal level test): ", slot(object, "decisions")[["slt.dec"]]))
  cat(paste0("\nNoise detected: ", slot(object, "decisions")[["shap.noisy"]]))
  cat(paste0("\nNoise level: ", slot(object, "noiselevel")))
  cat(paste0("\nLinearity: ", slot(object, "decisions")[["lrt.test"]]))
  bcg <- slot(object, "background")
  if (is.null(bcg)) {
    cat(paste0("\nBackground: not defined")) 
  } else {
    bcg <- paste0(bcg, collapse = ", ")
    cat(paste0("\nBackground: (", bcg, ")"))
  }
})

#der class ----------------------
setClass("der", contains = "matrix", representation(.Data = "matrix", 
                                                    method = "character"))


setMethod("summary", signature(object = "der"), function(object, digits = 0, print = TRUE) {
  data <- slot(object, ".Data")
  FDM <- data[data[, "d1y"] == max(data[, "d1y"]), "x"] 
  SDM <- data[data[, "d2y"] == max(data[, "d2y"]), "x"]
  SDm <- data[data[, "d2y"] == min(data[, "d2y"]), "x"]
  SDC <- sqrt(SDM * SDm)
  if (print) {
    cat(paste0("Smoothing method: ", slot(object, "method")))
    cat(paste0("\nFirst derivative maximum: ", round(FDM, digits = digits)))
    cat(paste0("\nSecond derivative maximum: ", round(SDM, digits = digits)))
    cat(paste0("\nSecond derivative minimum: ", round(SDm, digits = digits)))
    cat(paste0("\nSecond derivative center: ", round(SDC, digits = digits)))
  }
  res <- c(FDM, SDM, SDm, SDC)
  names(res) <- c("FDM", "SDM", "SDm", "SDC")
  invisible(res)
})

setMethod("show", signature(object = "der"), function(object) {
  print(slot(object, ".Data"))
})

setMethod("plot", signature(x = "der"), function(x, what = 1:3, add = FALSE,
                                                 legend = TRUE,
                                                 plot.colors = c("black", "red", "blue"), 
                                                 ...) {
  if (!all(what %in% 1:3)) 
    stop("'what' must contain values from set: 1, 2, 3.")
  
  if (length(unique(what)) != length(what)) 
    stop("'what' must contain unique values.")
  
  if (length(plot.colors) != 3) 
    stop("'plot.colors' must contain three colors.")
  
  #smallest and biggest fluorescence values
  ylims <- range(sapply(what + 1, function(i) {
    x[, i]
  }))
  
  if(!add) {
    plot(x = range(x[, 1]),  y = ylims, cex = 0, ...)
  }
  
  for (i in what)
    points(x[, c(1, i + 1)], col = plot.colors[i], pch = 20, type = "b")
  
  
  if (legend)
    legend("topleft", c("Raw data", "First derivative", "Second derivative")[what], 
           pch = rep(20,3), col = plot.colors)
})


#bg class, bg.max function -------------------------------
setClass("bg", contains = "matrix", representation(.Data = "matrix", 
                                                   bg.start = "numeric",
                                                   bg.stop = "numeric",
                                                   bg.corr = "numeric",
                                                   fluo = "numeric",
                                                   amp.stop = "numeric"))


setMethod("summary", signature(object = "bg"), function(object, print = TRUE) {
  if (print) {
    cat(paste0("Background start: ", slot(object, "bg.start")))
    cat(paste0("\nBackground stop: ", slot(object, "bg.stop")))
    cat(paste0("\nBackground correlation: ", slot(object, "bg.corr")))
    cat(paste0("\nEnd of the amplification reaction: ", slot(object, "amp.stop")))
    cat(paste0("\nFluorescence at the end of the amplification reaction: ", 
               round(slot(object, "fluo"), options("digits")[["digits"]])))
  }
  invisible(c(bg.start = slot(object, "bg.start"), 
              bg.stop = slot(object, "bg.stop"),
              bg.corr = slot(object, "bg.corr"),
              amp.stop = slot(object, "amp.stop"),
              fluo = slot(object, "fluo")))
})

setMethod("show", signature(object = "bg"), function(object) {
  print(slot(object, ".Data"))
})

setMethod("plot", signature(x = "bg"), function(x, what = 1:3, add = FALSE, 
                                                indicators = TRUE, 
                                                legend = TRUE, stan.labs = TRUE, 
                                                plot.colors = c("black", "red", "blue"), 
                                                ...) {
  if (stan.labs) {
    plot(new("der", .Data = x, method = "supsmu"), what = what, 
         add = add, legend = FALSE, plot.colors = plot.colors, xlab = "Cycle", 
         ylab = "Fluorescence", ...)
  } else {
    plot(new("der", .Data = x, method = "supsmu"), what = what, 
         add = add, legend = FALSE, plot.colors = plot.colors, ...)
  }
  
  if (indicators) {
    abline(v = slot(x, "bg.start"))
    text(slot(x, "bg.start"), 0.2, "Background start", pos = 4)
    abline(v = slot(x, "bg.stop"), col = "blue")
    text(slot(x, "bg.stop"), 0.25, "Background stop", pos = 4, col = "blue")
    abline(v = slot(x, "amp.stop"), col = "green")
    text(slot(x, "amp.stop"), 0.3, "Plateau transition", pos = 4, col = "green")
  }
  if (legend)
    legend("topleft", c("Raw data", "First derivative", "Second derivative")[what], 
           pch = rep(20,3), col = plot.colors)
})



#refMFI class, MFIaggr function -------------------------------
setClass("refMFI", contains = "matrix", representation(.Data = "matrix", 
                                                       density = "density", 
                                                       qqnorm.data = "data.frame",
                                                       stats = "numeric"))

setMethod("show", signature(object = "refMFI"), function(object) {
  print(slot(object, ".Data"))
})

setMethod("qqnorm", signature(y = "refMFI"), function(y, main = "Normal Q-Q Plot",
                                                      xlab = "Theoretical Quantiles",
                                                      ylab = "Sample Quantiles",
                                                      plot.it = TRUE, datax = FALSE, ...) {
  qqnorm(unlist(slot(y, "qqnorm.data")), main = main, xlab = xlab,
         ylab = ylab, plot.it = plot.it, datax = datax)
})

setMethod("qqline", signature(y = "refMFI"), function(y, datax = FALSE, 
                                                      distribution = qnorm,
                                                      probs = c(0.25, 0.75), qtype = 7, ...) {
  qqline(unlist(slot(y, "qqnorm.data")), datax = datax, distribution = distribution,
         probs = probs, qtype = qtype)
})

setMethod("summary", signature(object = "refMFI"), function(object, print = TRUE) {
  stats <- slot(object, "stats")
  if (print) {
    cat(paste0("Mean: ", stats[1]))
    cat(paste0("\nMedian: ", stats[2]))
    cat(paste0("\nStandard deviation: ", stats[3]))
    cat(paste0("\nMedian absolute deviation: ", stats[4]))
  }
  invisible(c(mean = stats[1], 
              median = stats[2],
              sd = stats[3],
              mad = stats[4]))
})


setMethod("plot", signature(x = "refMFI"), function(x, CV = FALSE, type = "p", 
                                                    pch = 19, length = 0.05, 
                                                    col = "black") {
  # store the default plot parameters  
  default.par <- par("fig")
  
  res <- slot(x, ".Data")
  res.dens <- slot(x, "density")
  qqnorm.data <- unlist(slot(x, "qqnorm.data"))
  llul <- rownames(slot(x, "qqnorm.data"))
  stats <- slot(x, "stats")
  ncol_y <- ncol(slot(x, "qqnorm.data"))
  
  #Plot the Coefficient of Variance
  layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE))
  
  if (CV) {
    plot(res[, 1], res[, 4], xlab = "Cycle", ylab = "CV", 
         type = type, pch = pch, col = col,
         main = paste0("ROI samples: ", ncol_y, "\n",
                      "ROI mean: ", stats[1], " +- ", stats[3], "\n",
                      "ROI median: ", stats[2], " +- ", stats[4]
		)
    )
    
    # Add a range for the ROI
    abline(v = llul, col = "lightgrey", lwd = 1.25)
    #Plot the location with error bars.
    
    # "Calculate" the Quantile-Quantile plots and density plots
    # and plot the results
    
    plot(res.dens, xlab = "RFU", main = paste0("Cycle ", 
                                              llul[1], " to ", llul[2], 
                                              "\n", "bw ", 
                                              round(res.dens$bw, 3), 
                                              "\n", "N ", res.dens$n
                                        )
    )
    
  } else {
    plot(res[, 1], res[, 2], ylim = c(min(res[, 2] - res[, 3]), 
                                      max(res[, 2] + res[, 3])), 
                                      xlab = "Cycle", ylab = "MFI", 
                                      type = type, pch = pch, col = col,
         main = paste0("ROI samples: ", ncol_y, "\n",
                      "ROI mean: ", stats[1], " +- ", stats[3], "\n",
                      "ROI median: ", stats[2], " +- ", stats[4]
                      )
    )
    abline(v = llul, col = "lightgrey")
    
    arrows(res[, 1], res[, 2] + res[, 3], res[, 1], 
           res[, 2] - res[, 3], angle = 90, code = 3, 
           length = length, col = col)
    deviation.measure <- strsplit(colnames(res)[3], "(", fixed = TRUE)[[1]][2]
    deviation.measure <- substr(deviation.measure, 1, nchar(deviation.measure) - 1)
    mtext(paste0("Deviation: ", deviation.measure), 4)
    
    
    plot(res.dens, xlab = "RFU", main = paste("ROI cycle ", 
                                              llul[1], " to ", llul[2], 
                                              "\n", "bw ", 
                                              round(res.dens[["bw"]], 3), 
                                              "\n", "N ", res.dens[["n"]]
                                        )
    )
    
  }
  # Analysis of the quantiles
  qqnorm(x)
  qqline(x)
  
  # Restore default graphic parameters
  par(fig = default.par, new = FALSE)
})