plotCurves <- function(x, y, cyc = 1, fluo = 2:ncol(y), nrow = ceiling(sqrt(ncol(y))), 
                       CPP = FALSE, ...) {
  testxy(x, y, length = FALSE)
  
  if(!(class(y) %in% c("matrix", "data.frame"))) {
    stop("'y' must be matrix or data.frame.")
  }
  
  
  if(CPP) {
    cpp.res <- apply(y, 2, function(i) CPP(x, i)[["y.norm"]])
    #y <- apply(y, 2, normalizer)
  }
  
  if(ncol(y) %% nrow != 0) {
    new.columns <- nrow - (ncol(y) %% nrow)
    y <- cbind(y, matrix(rep(NA, new.columns*nrow(y)), 
                         ncol = new.columns))
    colnames(y)[(ncol(y) - new.columns + 1):ncol(y)] <- rep("Empty", new.columns)
    if(CPP)
      cpp.res <- cbind(cpp.res, matrix(rep(NA, new.columns * nrow(y)), 
                                 ncol = new.columns))
  }
  
  lay.matrix <- matrix(1L:ncol(y), nrow = nrow)
  
  layout(lay.matrix)
  
  lefts <- lay.matrix[, 1]
  
  bottoms <- lay.matrix[nrow(lay.matrix), ]
  
  
  old.oma <- par("oma")
  old.mar <- par("mar")
  par(oma = c(5, 4, 4, 2))
  par(mar = c(0, 0, 0, 0))
  
  x.lim = range(x)
  y.lim = range(y, na.rm = TRUE)
  if(CPP)
    y.lim <- range(cbind(y, cpp.res), na.rm = TRUE)
  
  
  curve.names <- colnames(y)
  sapply(1L:ncol(y), function(i) {
    plot(x, y[, i], xlim = x.lim, ylim = y.lim,  
         xaxt = "n", yaxt = "n", ylab = "", xlab = "", ...)
    res.NA <- which(is.na(y[, i]))
    rug(res.NA, col = 2, lwd = 1.5)
    ifelse(sum(res.NA) > 0, bg <- 2, bg <- 3)
    legend("topleft", curve.names[i], bg = bg)
    if(i %in% lefts)
      axis(2)
    if(i %in% bottoms)
      axis(1)
    if(CPP)
      lines(x, cpp.res[, i], col = "red")
  })
  
  par(oma = old.oma)
  par(mar = old.mar)
  
  invisible()
}