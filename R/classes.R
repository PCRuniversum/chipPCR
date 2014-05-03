setGeneric("summary")
setGeneric("plot")

#just for writing comfort, self explanatory
setClassUnion("numericOrNULL",c("numeric","NULL"))

#amptester function
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


#bg.max function
setClass("bg", representation(d = "data.frame", 
                              d1 = "data.frame",
                              delta = "numeric",
                              delta1 = "numeric",
                              bg.start = "numeric",
                              bg.stop = "numeric",
                              bg.corr = "numeric",
                              fluo = "numeric",
                              amp.stop = "numeric",
                              input = "data.frame"))


setMethod("summary", signature(object = "bg"), function(object) {
  #TODO smart way to present deltas' values
  cat(paste0("\nBackground start: ", slot(object, "bg.start")))
  cat(paste0("\nBackground stop: ", slot(object, "bg.stop")))
  cat(paste0("\nBackground correlation: ", slot(object, "bg.corr")))
  cat(paste0("\nEnd of the amplification reaction: ", slot(object, "amp.stop")))
  cat(paste0("\nFluoercence at the end of the amplification reaction: ", 
             round(slot(object, "fluo"), options("digits")[["digits"]])))
})


setMethod("plot", signature(x = "bg"), function(x) {
  #smallest and biggest fluorescence values 
  ylims <- range(sapply(list(slot(x, "d")[2], 
                       slot(x, "d1")[2], 
                       slot(x, "input")[2]),
                  range))
  
  plot(slot(x, "input"), xlim = range(slot(x, "input")[1]), 
       ylim = ylims, xlab = "Cycles", 
       ylab = "Fluorescence", 
       main = "Estimation of the Background Range\n in Absence of Noise", 
       type = "b", pch = 20)
  
  points(slot(x, "d"), col = "red", type = "b", pch = 20)
  points(slot(x, "d1"), col = "blue", type = "b", pch = 20)
  abline(v = slot(x, "bg.start"))
  text(slot(x, "bg.start"), 0.2, "Background start", pos = 4)
  abline(v = slot(x, "bg.stop"), col = "blue")
  text(slot(x, "bg.stop"), 0.25, "Background stop", pos = 4, col = "blue")
  abline(v = slot(x, "amp.stop"), col = "green")
  text(slot(x, "amp.stop"), 0.3, "Plateau transition", pos = 4, col = "green")
  legend("topleft", c("Raw data", "First derivative", "Second derivative"), 
         pch = rep(20,3), col = c(1,2,4))
})