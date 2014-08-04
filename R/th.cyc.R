th.cyc <-
function(x, y, r = 2500, auto = FALSE) {
# Sanity test for input values
testxy(x, y, length = FALSE)
# Rearrange data for further processing
xy <- data.frame(x = x, y = y)

# Determin type of threshold calculation
if (auto) r <- quantile(y[1:10], 0.1) + 3 * mad(y[1:10]) else r <- r

# Helper function to determine the number of neighbours for the regression
th.est <- function(xy, r = r, n) {
    # Fetch the neighbours of the cycle and fluorescence values at the threshold
    # fluorescence.

    xy.out <- rbind(tail(xy[xy[, 2] <= r, ], n),
		    head(xy[xy[, 2] >= r, ], n)
		    )

    # Determine with a quaratic polynome the equantion for the neighbours of the 
    # cycle and fluorescence values at the threshold fluorescence.

    xy.lm <- lm(xy.out[, 2] ~ xy.out[, 1] + I(xy.out[, 1]^2))

    # summary of statistical values of the fit
    out <- list(summary = summary(xy.lm), values = xy.out)
}
    # Actually used number of neighbours
    n <- seq(2,8,1)
    
    # List of all regression results for all tested regressions with different 
    # numbers of neighbours
    res.th.est <- lapply(n, function(n) {th.est(xy, r = r, n)})
        
    # Results of the selection criterium R suquared
    res.r.squ <- data.frame(r.sqared = sapply(1:length(n), 
			    function(i) res.th.est[[i]][[1]]$r.squared), 
			    n = n)
    
    # Result of the optimal regression
    xy.sum <- res.th.est[[which(res.r.squ[, 1] == max(res.r.squ[, 1]))]]

    # Extract the coefficients of the regression.
    a <- xy.sum[[1]][["coefficients"]][3,1]
    b <- xy.sum[[1]][["coefficients"]][2,1]
    c <- xy.sum[[1]][["coefficients"]][1,1]

    # Calculate the exact Ct value at the user defined fluorescence threshold.
    x.cal <- (-b/a)/2 + sqrt(((-b/a)/2)^2 - (c - r)/a)
    
# Create the output fot the exact Ct value, the regression and the neighbours 
# of the cycle and fluorescence values at the threshold fluorescence.

out <- list(Results = rbind(cyc.th = x.cal, atFluo = r), stats = xy.sum, 
	    Input = xy.sum$values)
out
}
