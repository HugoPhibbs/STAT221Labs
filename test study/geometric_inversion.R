geometric_inv_cdf <- function(u, p) {
    frac <- (log(1 - u) / log(1 - p))
    return(frac - 1)
}

geometric_pdf = function(x, p) {
    return(p*(1-p)^x)
}

p <- 0.3
u_sample <- runif(10000, 0, 1)
x_sample <- geometric_inv_cdf(u_sample, p)
hist(x_sample, col = "Blue", breaks = 1000, freq = FALSE)

x_sample_actual = seq(0, 20)
u_sample_actual = geometric_pdf(x_sample_actual, p)
lines(x_sample_actual, u_sample_actual, col = "red")



