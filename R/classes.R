setGeneric("summary")

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
                              amp.stop = "numeric"))


setMethod("summary", signature(object = "bg"), function(object) {
  #TODO smart way to present deltas' values
  cat(paste0("\nBackground start: ", slot(object, "bg.start")))
  cat(paste0("\nBackground stop: ", slot(object, "bg.stop")))
  cat(paste0("\nBackground correlation: ", slot(object, "bg.corr")))
  cat(paste0("\nEnd of the amplification reaction: ", slot(object, "amp.stop")))
  cat(paste0("\nFluoercence at the end of the amplification reaction: ", 
             round(slot(object, "fluo"), options("digits")[["digits"]])))
})