plotCurves <- function(x, y, cyc = 1, fluo = 2:ncol(x), nrow = 4, ...) {
  #testxy(x, y, length = FALSE)
  
  if(!is.null(ncol(y))) {
    if(ncol(y) %% nrow != 0) {
      new.columns <- nrow - (ncol(y) %% nrow)
      y <- cbind(y, matrix(rep(NA, new.columns*nrow(y)), 
			  ncol = new.columns))
      colnames(y)[(ncol(y) - new.columns + 1):ncol(y)] <- rep(NA, new.columns)
    }
  
  lay.matrix <- matrix(1L:ncol(y), nrow = nrow)
  
  layout(lay.matrix)
  
  lefts <- lay.matrix[, 1]
  
  bottoms <- lay.matrix[nrow(lay.matrix), ]
  
   
  old.oma <- par("oma")
  old.mar <- par("mar")
  par(oma = c(5, 4, 4, 2))
  par(mar = c(0, 0, 0, 0))
  
  curve.names <- colnames(y)
  sapply(1L:ncol(y), function(i) {
    plot(x, y[, i], xlim = range(x), ylim = range(y, na.rm = TRUE),  
         xaxt = "n", yaxt = "n", ylab = "", xlab = "", ...)
    res.NA <- which(is.na(y[, i]))
    rug(res.NA, col = 2, lwd = 1.5)
    ifelse(sum(res.NA) > 0, bg <- 2, bg <- 3)
    legend("topleft", curve.names[i], bg = bg)
    if(i %in% lefts)
      axis(2)
    if(i %in% bottoms)
      axis(1)
  })
  
  par(oma = old.oma)
  par(mar = old.mar)
  } else {
	  warning("Onyl one y column.")
  }
}
