f <- function(x) {
    return((1 / sqrt(2 * pi)) * (exp((-x^2) / 2)))
}

g <- function(x) {
    return(1 / (pi * (1 + x^2)))
}

c <- (sqrt(2 * pi)) * exp(-1 / 2)

# Part a
x_sample <- seq(0, 5, 0.01)
plot(x_sample, c * g(x_sample), col = "blue")
lines(x_sample, f(x_sample), col = "red")

# Part b
n <- 10000
y_sample <- rcauchy(n)
u <- runif(n, 0, 1)
accept <- u < (f(y_sample) / (c * g(y_sample)))
x_sample <- y_sample[accept]
hist(x_sample, breaks = 100, freq =  FALSE, col = "blue")

# Part c
x_sample_actual <- seq(-3, 3, 0.1)
p_sample_actual <- f(x_sample_actual)
lines(x_sample_actual, p_sample_actual, col = "red")

