library(boot)

# Our sample size
n = 1000

# Part 1, Bootstrapping with normal distribution

# Question 1
# Create a standard normal sample
x_norm_sample = rnorm(n=n, mean=0, sd=1)

# Estimating quantiles from sample
quantile(x = x_norm_sample, probs = c(0.10, 0.25, 0.5, 0.75, 0.90))

# Vector of quantile probabilities
quant_probs = c(0.5, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95)

# Getting theoretical quantiles
t_quantiles_norm = setNames(qnorm(quant_probs, mean = 0, sd = 1), c("5%", "10%", "25%", "50%", "75%", "90%", "95%"))

# Question 2
# Function to calculate quantiles for a sample
quant_stat = function(x, indices, quant_probs){
  x_ = x[indices]
  return(quantile(x_, probs = quant_probs))
}

# Number of resamples that we will do 
R = 10000

# Bootstrapping our quantiles
boot_quantile_norm = boot(data = x_norm_sample, R = R, statistic = quant_stat, quant_probs = quant_probs)

boot_cis_norm = matrix(nrow = length(quant_probs), ncol = 2)

# Getting confidence intervals
for (i in 1:length(quant_probs)){
  print('')
  print('')
  print(quant_probs[i])
  boot_cis_norm[i, ] = boot.ci(boot.out = boot_quantile_norm, index = i, type = "perc", conf = 0.9)$percent[4:5]
  print(boot_cis_norm[i, ])
}


# Question 3
# Creating a histogram plot for each of these quantiles
for (i in 1:length(quant_probs)){
  hist(x = boot_quantile_norm$t[, i], freq = FALSE, main = quant_probs[i], breaks = 30, col = "aquamarine")
  abline(v = c(boot_cis_norm[i, ], t_quantiles_norm))
  lines(density(x = boot_quantile_norm$t[, i], bw = 0.05), col = "red")
}


# Part 2/question 4

# Repeating above for the exponential distribution
x_exp_sample = rexp(n=n, rate=1)

# Getting quantiles of theoretical exponential distribution
t_quantiles_exp = qexp(p = quant_probs, rate = 1)

# Creating an standard exponential sample
boot_quantile_exp = boot(data = x_exp_sample, R = R, statistic = quant_stat, quant_probs = quant_probs)

# Vector to hold confidence intervals for each quantiles
boot_cis_exp = matrix(nrow = length(quant_probs), ncol = 2)

# Getting confidence intervals
for (i in 1:length(quant_probs)){
  print('')
  print('')
  print(quant_probs[i])
  boot_cis_exp[i,] = boot.ci(boot.out = boot_quantile_exp, index = i, type = "perc", conf = 0.9)$percent[4:5]
  print(boot_cis_exp[i,])
}


# Question 3
# Creating a histogram plot for each of these quantiles
# Add a kernel density estimation to get general gist of distribution
for (i in 1:length(quant_probs)){
  hist(x = boot_quantile_exp$t[, i], freq = FALSE, main = quant_probs[i], breaks = 30, col = "aquamarine")
  abline(v = c(boot_cis_exp[i,], t_quantiles_exp[i]))
  lines(density(x = boot_quantile_exp$t[, i], bw = "nrd"), col = "red")
}

