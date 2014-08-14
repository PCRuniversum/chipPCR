plot.curves <- function(x, y, cyc = 1, fluo = 2:ncol(x), nrow) {
  #testxy(x, y, length = FALSE)
  
  lay_matrix <- matrix(1L:ncol(y), nrow = nrow)
  
  layout(lay_matrix)
  
  tops <- lay_matrix[1, ]
  bottoms <- lay_matrix[nrow(lay_matrix), ]
  lefts <- lay_matrix[, 1]
  rights <- lay_matrix[, ncol(lay_matrix)]
  
  mar_list <- lapply(1L:ncol(y), function(i) rep(0, 4))
  
  for (i in tops)
    mar_list[[i]] <- c(0, 0, 4, 0)
  for (i in bottoms)
    mar_list[[i]] <- c(5, 0, 0, 0)
  for (i in rights)
    mar_list[[i]] <- c(0, 0, 0, 2)
  for (i in lefts)
    mar_list[[i]] <- c(0, 4, 0, 0)
  
  mar_list[[lay_matrix[1, 1]]] <- c(0, 4, 4, 0)
  mar_list[[lay_matrix[nrow(lay_matrix), 1]]] <- c(5, 4, 0, 0)
  mar_list[[lay_matrix[1, ncol(lay_matrix)]]] <- c(0, 0, 4, 2)
  mar_list[[lay_matrix[nrow(lay_matrix), ncol(lay_matrix)]]] <- c(5, 0, 0, 2)

  sapply(1L:ncol(y), function(i) {
    old_mar <- par("mar")
    par(mar = mar_list[[i]])
    plot(x, y[, i], ylim = range(y), xaxt = "n", yaxt = "n", ylab = "", xlab = "")
    if(i %in% lefts)
      axis(2)
    if(i %in% bottoms)
      axis(1)
    par(mar = old_mar)
  })
}

#example
#plot.curves(VIMCFX96_60[, 1], VIMCFX96_60[, 2:13])
