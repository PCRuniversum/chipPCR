th.cyc <-
function(x, y, r = 2500, auto = FALSE) {

xy <- data.frame(x = x, y = y)
if (auto) r <- quantile(y[1:10], 0.1) + 3 * mad(y[1:10]) else r <- r
xy.out <- rbind(tail(xy[xy[, 2] <= r, ], 2),
		head(xy[xy[, 2] >= r, ], 2))

		
xy.lm <- lm(xy.out[, 2] ~ xy.out[, 1] + I(xy.out[, 1]^2))
xy.coeff <- coefficients(xy.lm)

c <- xy.coeff[1]
b <- xy.coeff[2]
a <- xy.coeff[3]

x.cal <- (-b/a)/2 + sqrt(((-b/a)/2)^2 - (c - r)/a)
out <- list(Results = rbind(cyc.th = x.cal, atFluo = r), fit = xy.lm, Input = xy.out)
out
}
