f <- function(x) {
    return((2 / sqrt(2 * pi)) * (exp(-1 * (x^2) / 2)))
}

g <- function(x) {
    return(exp(-x))
}

c <- (sqrt(2 / pi) * exp(1 / 2))

# Part b
n <- 10000
u_sample <- runif(n, 0, 1)
y_sample <- rexp(n, rate = 1)
accept <- u < (f(y_sample) / (c * g(y_sample)))
x_sample <- y_sample[accept]

hist(x_sample, breaks = 100, col = "blue", freq = FALSE, ylim = c(0, 1.5))

# Part c
x_sample_actual <- seq(0, 4, 0.1)
lines(x_sample_actual, f(x_sample_actual), co = "red")

lines(x_sample_actual, c * g(x_sample_actual), col = "green")

# Part d
coefs <- sample(c(-1, 1), length(x_sample), replace = TRUE)
x_sample <- x_sample * coefs
hist(x_sample, col = "green", freq = FALSE, breaks = 1000)

# Part e
x_sample_normal = seq(-4, 4, 0.01)
lines(x_sample_normal, dnorm(x_sample_normal), col = "purple")