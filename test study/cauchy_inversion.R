cauchy_inv_cdf <- function(u) {
    return(tan(pi * (u - 1 / 2)))
}

cauchy_pdf <- function(x) {
    return(1 / (pi * (1 + x^2)))
}

n <- 1000
u_sample <- runif(1000, 0, 1)
x_sample <- cauchy_inv_cdf(u_sample)
hist(x_sample, breaks = 1000, freq = FALSE, xlim = c(-100, 100), col = "blue")

x_sample_actual <- seq(-250, 250)
u_sample_actual <- cauchy_pdf(x_sample_actual)
lines(x_sample_actual, u_sample_actual, col = "red")

# Matches pretty much prefectly

