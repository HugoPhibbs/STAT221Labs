library(boot)
library(MASS)

# Q1, Create gamma distribution sample
n  = 100


alpha = 2
beta = 3

g_sample = rgamma(n, shape = alpha, scale = beta)


# Q2, Create histogram for g_sample

hist(g_sample, freq = FALSE, col = "aquamarine")

# Q3, Use a non-parametric bootstrap for mean and plot

mean_stat = function(x, indices) {
  x_  = x[indices]
  return(mean(x_))
}

R = 10000

mean_boot_sample = boot(data = g_sample, statistic = mean_stat, R = R)

hist(mean_boot_sample$t, 
     col = "coral", 
     freq = FALSE, 
     xlab = "Bootstrapped mean", 
     main = "Boostrapped mean for gamma distribution"
     )

# Q4, find a 95% conf interval for the mean, using percentiles
interval_mean = boot.ci(boot.out = mean_boot_sample, conf = 0.95, type = "perc")
print(interval_mean)

# Q5, Use a nonparametric bootstrap to find the confidence interval for the variance

var_stat = function(x, indices){
  return(var(x[indices]))
}

var_boot_sample = boot(data = g_sample, statistic = var_stat, R=R)

interval_var = boot.ci(boot.out = var_boot_sample, conf = 0.95, type = "perc")
print(interval_var)

# Q6 Use a nonparametric bootstrap to find the confidence interval for the standard deviation

# Finding directly from var, see if this works! # YUP it does!, to be expected
quantile(x = sqrt(var_boot_sample$t), probs = c(0.05, 0.95))

sd_stat = function(x, indices){
  return(sd(x[indices]))
}

sd_boot_sample = boot(data = g_sample, statistic = sd_stat, R=R)

interval_sd = boot.ci(boot.out = sd_boot_sample, conf = 0.95, type = "perc")
print(interval_sd)

# Q7 Use nonparametric bootstrap to find confidence intervals for the shape and the scale parameter

params_stat = function(x, indices){
  # Returns estimates of the shape and scale of a gamma distr sample
  x_ = x[indices]
  estimate = fitdistr(x_, densfun = "gamma")$estimate
  return(c(estimate["shape"], 1/estimate["rate"]))
}

fitdistr(g_sample, densfun = "gamma")$estimate["shape"]

shape_scale_boot_sample = boot(data = g_sample, statistic = params_stat, R = R)

boot.ci(boot.out = shape_scale_boot_sample, index = 1, type = "perc", conf = 0.95)
boot.ci(boot.out = shape_scale_boot_sample, index = 2, type = "perc", conf = 0.95)

# Q8 Use a parametric bootstrap, making the assumption that X ∼ Gamma(α, β), to find a confidence
#    interval for the mean and compare to it to that found using the nonparametric bootstrap.

params_est = fitdistr(g_sample, densfun = "gamma")$estimate
shape_est = params_est["shape"]
scale_est = 1/params_est["rate"]

gamma_gen = function(x, params){
  return(rgamma(length(x), shape = params[1], scale = params[2]))
}

mle = c(shape_est, scale_est)

mean_boot_sample_para = boot(data = g_sample, 
                             statistic = mean_stat, 
                             R=R, 
                             sim = "parametric", 
                             ran.gen = gamma_gen,
                             mle = mle)

hist(x = mean_boot_sample_para$t, col = "pink", freq = FALSE) # Looks good

boot.ci(boot.out = mean_boot_sample_para, conf = 0.95, type = "perc") # Looks good

# Q9 Use a parametric bootstrap to find confidence intervals for the variance, the shape and the scale
# parameter.

var_boot_sample_para = boot(data = g_sample, 
                            statistic = var_stat, 
                            R=R, 
                            sim = "parametric", 
                            ran.gen = gamma_gen,
                            mle = mle)

sd_boot_sample_para = boot(data = g_sample, 
                           statistic = sd_stat, 
                           R=R, 
                           sim = "parametric", 
                           ran.gen = gamma_gen,
                           mle = mle)

shape_scale_boot_sample_para = boot(data = g_sample, 
                                    statistic = params_stat, 
                                    R=R, 
                                    sim = "parametric", 
                                    ran.gen = gamma_gen,
                                    mle = mle)

boot.ci(boot.out = var_boot_sample_para, type = "perc")
boot.ci(boot.out = sd_boot_sample_para, type = "perc")
boot.ci(boot.out = shape_scale_boot_sample_para, type = "perc")

# All look good

