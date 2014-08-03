th.cyc <-
function(x, y, r = 2500, auto = FALSE) {
# Sanity test for input values
testxy(x, y, length = FALSE)
# Rearrange data for further processing
xy <- data.frame(x = x, y = y)

# Determin type of threshold calculation
if (auto) r <- quantile(y[1:10], 0.1) + 3 * mad(y[1:10]) else r <- r

# Fetch the neighbours of the cycle and fluorescence values at the threshold
# fluorescence.
xy.out <- rbind(tail(xy[xy[, 2] <= r, ], 2),
		head(xy[xy[, 2] >= r, ], 2)
		)

# Determine with a quaratic polynome the equantion for the neighbours of the 
# cycle and fluorescence values at the threshold fluorescence.

xy.lm <- lm(xy.out[, 2] ~ xy.out[, 1] + I(xy.out[, 1]^2))

# Extract the coefficients of the regression.
xy.coeff <- coefficients(xy.lm)
c <- xy.coeff[1]
b <- xy.coeff[2]
a <- xy.coeff[3]

# Calculate the exact Ct value at the user defined fluorescence threshold.
x.cal <- (-b/a)/2 + sqrt(((-b/a)/2)^2 - (c - r)/a)

# Create the output fot the exact Ct value, the regression and the neighbours 
# of the cycle and fluorescence values at the threshold fluorescence.

out <- list(Results = rbind(cyc.th = x.cal, atFluo = r), fit = xy.lm, 
	    Input = xy.out)
out
}
