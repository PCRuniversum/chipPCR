AmpSim <- function(cyc = c(1:35), b.eff = -25, bl = 0.05, ampl = 1, 
		   Cq = 20, noise = FALSE, nnl = 0.025, 
		   nnl.method = "constant")
{
  tmp.warn <- getOption("warn")
  options(warn = -1)
  # Test if x and y exist and have identical lengths.
  if (is.null(cyc)) 
    stop("Enter cycle values")
  if (nnl < 0 || nnl > 0.1) 
    stop("nnl must be within 0 and 0.1.")
   
  fluo <- bl + (ampl - bl)/(1 + exp(b.eff * (log(cyc) - log(Cq))))
  
  if (noise) {
  mean.noise <- mean(fluo) * nnl
  sd.noise <- sd(fluo) * nnl
  if (nnl.method == "increase") {
		NOISE <- sort(rnorm(length(fluo), 
				mean = mean.noise, 
				sd = sd.noise))
		}
  if (nnl.method == "decrease") {
		NOISE <- sort(rnorm(length(fluo), 
		mean = mean.noise, 
		sd = sd.noise), decreasing = TRUE)
		}
  if (nnl.method == "constant") {
		NOISE <- rnorm(length(fluo), 
				mean = mean.noise, 
				sd = sd.noise)
		}
  fluo <- fluo + NOISE
  res <- data.frame(cyc, fluo)
  } else (res <- data.frame(cyc, fluo))
  options(warn = tmp.warn)
  res
}