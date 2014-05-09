setGeneric("summary")
setGeneric("plot")

#just for writing comfort, self explanatory
setClassUnion("numericOrNULL",c("numeric","NULL"))

#amptest class, amptester function -------------------------------
setClass("amptest", contains = "numeric", representation(.Data = "numeric", 
                                                         decision = "character",
                                                         noiselevel = "numeric",
                                                         background = "numericOrNULL"))

setMethod("show", signature(object = "amptest"), function(object) {
  print(slot(object, ".Data"))
})

setMethod("summary", signature(object = "amptest"), function(object) {
  print(slot(object, ".Data"))
  cat(paste0("\nDecision: ", slot(object, "decision")))  
  cat(paste0("\nNoise level: ", slot(object, "noiselevel")))
  bcg <- slot(object, "background")
  if (is.null(bcg)) {
    cat(paste0("\nBackground: not defined")) 
  } else {
    bcg <- paste0(bcg, collapse = ", ")
    cat(paste0("\nBackground: (", bcg, ")")) 
  }
})


#bg class, bg.max function -------------------------------
setClass("bg", representation(d = "data.frame", 
                              d1 = "data.frame",
                              delta = "numeric",
                              delta1 = "numeric",
                              bg.start = "numeric",
                              bg.stop = "numeric",
                              bg.corr = "numeric",
                              fluo = "numeric",
                              amp.stop = "numeric",
                              raw.data = "data.frame"))


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


setMethod("plot", signature(x = "bg"), function(x, what = 1:3, add = FALSE, indicators = TRUE,
                                                legend = TRUE,
                                                plot.colors = c("black", "red", "blue"), 
                                                ...) {
  if (!all(what %in% 1:3)) 
    stop("'what' must contain values from set: 1, 2, 3.")
  
  if (length(unique(what)) != length(what)) 
    stop("'what' must contain unique values.")
  
  if (length(plot.colors) != 3) 
    stop("'plot.colors' must contain three colors.")
  
  #needed data frames
  all.dfs <- c("raw.data", "d", "d1")  
  
  #smallest and biggest fluorescence values
  ylims <- range(sapply(what, function(i) {
    range(slot(x, all.dfs[i])[2])
  }))
  
  if(!add) {
    plot(x = range(slot(x, "raw.data")[1]),  y = ylims, xlab = "Cycles", 
         ylab = "Fluorescence", cex = 0, ...)
    
    if (indicators) {
      abline(v = slot(x, "bg.start"))
      text(slot(x, "bg.start"), 0.2, "Background start", pos = 4)
      abline(v = slot(x, "bg.stop"), col = "blue")
      text(slot(x, "bg.stop"), 0.25, "Background stop", pos = 4, col = "blue")
      abline(v = slot(x, "amp.stop"), col = "green")
      text(slot(x, "amp.stop"), 0.3, "Plateau transition", pos = 4, col = "green")
    }
  }
  for (i in what) {
    points(slot(x, all.dfs[i]), col = plot.colors[i], type = "b", pch = 20)
  }
  
  if (legend)
    legend("topleft", c("Raw data", "First derivative", "Second derivative")[what], 
           pch = rep(20,3), col = plot.colors)
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

setMethod("plot", signature(x = "der"), function(x, what = 1:3, add = FALSE, indicators = TRUE,
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
    plot(x = range(x[, 1]),  y = ylims, xlab = "Cycles", 
         ylab = "Fluorescence", cex = 0, ...)
    
#     if (indicators) {
#       abline(v = slot(x, "bg.start"))
#       text(slot(x, "bg.start"), 0.2, "Background start", pos = 4)
#       abline(v = slot(x, "bg.stop"), col = "blue")
#       text(slot(x, "bg.stop"), 0.25, "Background stop", pos = 4, col = "blue")
#       abline(v = slot(x, "amp.stop"), col = "green")
#       text(slot(x, "amp.stop"), 0.3, "Plateau transition", pos = 4, col = "green")
#     }
  }
  for (i in what)
    points(x[, c(1, i + 1)], col = plot.colors[i], type = "b", pch = 20)
  
  
  if (legend)
    legend("topleft", c("Raw data", "First derivative", "Second derivative")[what], 
           pch = rep(20,3), col = plot.colors)
})
