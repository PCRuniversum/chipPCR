humanrater <-
function(x, repeats = 1) {
 sapply(1L:repeats, function(j) {
    sapply(2L:ncol(x), function(i) {
      plot(x[, 1], x[, i], main = i, type = "b", pch = 19, lwd = 2, 
	   xlab = "Cycle", ylab = "Fluorescence")
      res <- readline(prompt="y if amplification, a if ambigous, an n if not")
      # Add check for false input (not a, y or n) and repeat if wrong
      res
    })
  })
  if (ncol(res) >= 2) {
  check <- sapply(1L:nrow(res), function(k) {
	  ifelse(nrow(unique(data.frame(res[k, ]))) > 1, 
	  "missmatch", "correct")
	  }) } else {
	         check <- rep("not tested", nrow(res))
	  }
  data.frame(testresult = res, conformity = check)
}
