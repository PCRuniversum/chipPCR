humanrater <-
  function(x, repeats = 1, designations = c("y", "a", "n")) {
    if (!is.numeric(repeats) || length(repeats) > 1)
      stop("'repeats' must be a numeric vector of length 1.")
    if (repeats < 1)
      stop("'repeats' must be have value 1 or bigger")
    if (!is.character(designations) || length(designations) != 3)
      stop("'designations' must be a character vector of length 3.")
    prompt.line <- paste0("[", designations[1], 
                          "] if yes, [", designations[2],
                          "] if ambiguous, an [", designations[3],
                          "] if not: ")
    all.ratings <- sapply(1L:repeats, function(j) {
      if(repeats > 1)
        cat(paste0("Repeat:", j, "\n"))
      sapply(2L:ncol(x), function(i) {
        plot(x[, 1], x[, i], main = paste0("Experiment ", i), type = "b", pch = 19, lwd = 2, 
             xlab = "Cycle", ylab = "Fluorescence")
        #declare dummy variable - without it while loop does not work
        #the dummy cannot belong to the set of designations
        res <- "dummy which surely would not be a designation"
        while((!(res %in% designations))) {
          res <- readline(prompt = prompt.line)
          #check correctness
          if (!res %in% designations)
            cat("Wrong input. Try again.\n")
        }
        res
      })
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
