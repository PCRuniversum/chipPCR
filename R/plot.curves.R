plot.curves <- function(x, y, cyc = 1, fluo = 2:ncol(x), nrow, ...) {
  #testxy(x, y, length = FALSE)
  
  if(!ncol(y) %% 2 == 0) {y <- data.frame(y, Empty = rep(0, length(x)))}
  
  lay.matrix <- matrix(1L:ncol(y), nrow = nrow)
  
  layout(lay.matrix)
  
  tops <- lay.matrix[1, ]
  bottoms <- lay.matrix[nrow(lay.matrix), ]
  lefts <- lay.matrix[, 1]
  rights <- lay.matrix[, ncol(lay.matrix)]
  
  mar.list <- lapply(1L:ncol(y), function(i) rep(0, 4))
  
  for (i in tops)
    mar.list[[i]] <- c(0, 0, 4, 0)
  for (i in bottoms)
    mar.list[[i]] <- c(5, 0, 0, 0)
  for (i in rights)
    mar.list[[i]] <- c(0, 0, 0, 2)
  for (i in lefts)
    mar.list[[i]] <- c(0, 4, 0, 0)
  
  mar.list[[lay.matrix[1, 1]]] <- c(0, 4, 4, 0)
  mar.list[[lay.matrix[nrow(lay.matrix), 1]]] <- c(5, 4, 0, 0)
  mar.list[[lay.matrix[1, ncol(lay.matrix)]]] <- c(0, 0, 4, 2)
  mar.list[[lay.matrix[nrow(lay.matrix), ncol(lay.matrix)]]] <- c(5, 0, 0, 2)

  sapply(1L:ncol(y), function(i) {
    old.mar <- par("mar")
    par(mar = mar.list[[i]])
    plot(x, y[, i], ylim = range(y), xaxt = "n", yaxt = "n", ylab = "", xlab = "", ...)
    if(i %in% lefts)
      axis(2)
    if(i %in% bottoms)
      axis(1)
    par(mar = old.mar)
  })
}

#example
#plot.curves(VIMCFX96_60[, 1], VIMCFX96_60[, 2L:16], nrow = 2, type = "l")
