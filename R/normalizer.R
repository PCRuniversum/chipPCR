normalizer <- function(y, method.norm = "none", qnL = 0.03) {
  if (qnL <= 0.001 || qnL >= 0.999) 
    stop("qnL must be within 0.001 and 0.999.")
  
  # Select a method for the normalization
  
  method.norm <- tolower(method.norm)
  if (grepl(method.norm, "none"))
    method.norm <- "none"
  if (grepl(method.norm, "luqn")) 
    method.norm <- "luqn"
  if (grepl(method.norm, "minmax"))
    method.norm <- "minmax"
  if (grepl(method.norm, "max"))
    method.norm <- "max"
  if (grepl(method.norm, "zscore"))
    method.norm <- "zscore"
  if (!(method.norm %in% c("none", "luqn", "minmax", "max", "zscore")))
    stop("Invalid method chosen.")
  # Test meaningfulness of qnL
  
  switch(method.norm,
         none = do.call(function(y) y, c(list(y = y))),
         minmax = do.call(function(y) (y - min(y)) / (max(y) - min(y)), 
                          c(list(y = y))),
         max = do.call(function(y) (y / max(y)), 
                       c(list(y = y))),
         luqn = do.call(function(y, qnL) (y - quantile(y, qnL)) / 
                          (quantile(y, 1  - qnL) - quantile(y, qnL)), 
                        c(list(y = y, qnL = qnL))),
         zscore = do.call(function(y) (y - mean(y)) / sd(y), 
                          c(list(y = y)))
  )	
}