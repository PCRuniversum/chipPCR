humanrater <-
  function(x, cyc = 1, repeats = 1, 
           designations = list(y = "yes", a = "ambiguous", n = "not"), 
           shuffle = TRUE, ...) {
    if (!is.numeric(repeats) || length(repeats) > 1)
      stop("'repeats' must be a numeric vector of length 1.")
    if (repeats < 1)
      stop("'repeats' must be have value 1 or bigger")
    if (length(designations) < 2)
      stop("'designations' must have length at least 2.")
    if (!is.list(designations))
      stop("'designations' must be a list.")
    #     if (!is.character(designations) || length(designations) != 3)
    #       stop("'designations' must be a character vector of length 3.")
    allowed.symbols <- names(designations)
    
    if (any(table(allowed.symbols) > 1))
      stop("Do not use repeated names.")
    if ("" %in% allowed.symbols)
      stop("All elements of 'designations' list must be named.")
    prompt.line <- paste(sapply(1L:length(designations), function(i)
      paste0("[", allowed.symbols[i], "] if ", designations[[i]])), collapse = ", ")
    
#     x.bg <- lapply(2L:ncol(x), function(i) {x[, i] / quantile(na.omit(x[, i]), .99)})
#     
#     x.bg.cbind <- do.call(cbind, x.bg)
#     
#     colnames(x.bg.cbind) <- colnames(x[, -1])
#     
#     x <- cbind(x[, 1], x.bg.cbind)
    
   # Determine the range of the ordinate values
   y.quantiles <- quantile(x[, -1], c(0.25, 0.50, 0.75), na.rm=TRUE)

    all.ratings <- sapply(1L:repeats, function(j) {
      if(repeats > 1)
        cat(paste0("Repeat:", j, "\n"))
      rating.order <- 2L:ncol(x)
      if(shuffle)
        rating.order <- sample(rating.order)

     # Determine the total number of repeats
     total.repeats <- length(rating.order) * repeats

     # Assign all repeats to a matrix for proper tracking of the current index
      matrix.counts <- matrix(1L:total.repeats, ncol = repeats)
      
      y.range <- range(x[, -1], na.rm=TRUE)
      
	all.ratings <- sapply(1L:length(rating.order), function(i) {

     	plot(x[, 1], x[, rating.order[i]], main = paste0("Experiment ", rating.order[i], "\n", "Repeat ", j,
     	  " of ", total.repeats, " experiments in total (", round(matrix.counts[i, j] / total.repeats * 100, 2), " %)"), 
	     type = "b", pch = 19, lwd = 2, xlab = "Cycle", ylim = c(y.range[1], y.range[2]), ylab = "Fluorescence", ...)
	
	# Add lines of y value quantiles (25%, 50% and 75%)
	abline(h = y.quantiles, col = "grey", lty=c(2,1,2), lwd=c(0.5,1,0.5))
        
	#declare dummy variable - without it while loop does not work
        #the dummy cannot belong to the set of designations
        res <- "dummy which surely would not be a designation"
        while((!(res %in% allowed.symbols))) {
          res <- readline(prompt = prompt.line)
          #check correctness
          if (!res %in% allowed.symbols)
            cat("Wrong input. Try again.\n")
        }
        res
      })
      all.ratings[order(rating.order)]
    })
    if (repeats > 1) {
      #check conformity of a human assessment
      check <- apply(all.ratings, 1, function(k) {
        ifelse(length(unique(k)) == 1, TRUE, FALSE)
      }) 
    } else {
      check <- rep("not tested", nrow(all.ratings))
    }
    data.frame(test.result = all.ratings, conformity = check)
  }
