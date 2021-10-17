set.seed(96000464)

# ----  Creating our samples ------ #
nx <- 4
ny <- 4
x <- rnorm(n = nx, mean = 0, sd = 1)
y <- rnorm(n = ny, mean = 1.5, sd = 2)

# 1. Performing a one sided test, if E(X) < E(Y)

# Let the null hypo H0 = E(x) >= E(Y), under level of sig 0.05
alpha <- 0.05
t_test <- t.test(alternative = "less", x = x, y = y)
# A note on t_test,
t_test$p.value < alpha # If true, we accept the null hypothesis
# Depends on what we set the seed as....

# 2. Performing a permutation test

# Let the null hypo H0 = E(x) >= E(Y)
# We suppose this null hypothesis bc this means that we can mix X and Y
# Together


# Base-line t value
base_t_stat <- t.test(x, y, alternative = "less")$statistic

# How many permuation trials we are going to do
n <- 1000

# Vector to hold t values of permuted samples
t_stat_vals <- numeric(n)

# Combine x and y into one vector
xy <- c(x, y)

# Vector to split a shuffled xy sampled and then seperate it
include <- c(rep(TRUE, nx), rep(FALSE, ny))

# Create t values for each shuffed xy sample
for (i in 1:n) {
    xy_shuffle <- sample(xy)
    x_temp <- xy_shuffle[include]
    y_temp <- xy_shuffle[!include]
    t_stat_vals[i] <- t.test(x_temp, y_temp, alternative= "less")$statistic
}

# Get the p value, ie at what percentage does
# the base-line p value sit at for other vals?
p_value <- mean(t_stat_vals > base_t_stat)

# P value is is < 0.05, reject null hypothesis

hist(t_stat_vals, breaks = 30, freq = FALSE)

# Plotting the student t density on top!
x_vals = seq(-5, 5, length = 100)
t_density = dt(x = x_vals, df = t_test$parameter)
lines(x_vals, t_density, col = "red")
# We can accept the null hypothesis, that E(X) < E(Y)

# 3. Comparing sampling distributions
# If we clock nx = ny = 100 we get a much stronger t_value, the p value effectively becomes zero,
# This is not suprising bc central limit theorem applies and says
# That the distribution will converge to the true sd and mean as n becomes large=