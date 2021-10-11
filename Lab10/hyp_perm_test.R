set.seed(96000467)

# Creating our samples
x_n = 4
y_n = 4
x = rnorm(mean = 0, sd = 1, n = x_n)
y = rnorm(mean = 1.5, sd = 2, n = y_n)

# Level of significance
alpha = 0.05

# Question 1
# Performing a test
base_test = t.test(x=x, y=y, alternative = "less")
base_test_stat = t.test(x=x, y=y, alternative = "less")$statistic

base_test_stat
# Question 2
# Performing a permutation test

include = c(rep(TRUE, x_n), rep(FALSE, y_n))
N = 10000
perm_test_stats = numeric(N)

for (i in 1 : N){
  x_y = c(x, y)
  x_y_shuffled = sample(x_y)
  temp_x = x_y_shuffled[include]
  temp_y = x_y_shuffled[!include]
  perm_test_stats[i] = t.test(x = temp_x, y = temp_y, alternative = "less")$statistic
}

perm_test_stats

p_val = mean(perm_test_stats > base_test_stat)

print(p_val)
print(p_val < alpha)
# Accept the null hypothesis H1:mu_x >= mu_y. This is likely due to such small samples
# of x and y1

# Question 3
hist(perm_test_stats, breaks = 10, freq = FALSE)
     
# Question 4
# Perform perm test for a range of sample sizes

# What explains the two peaks???
abline(v=base_test_stat, col = "red")

n_vals = c(5, 10, 25, 50, 100)

for (j in 1 : length(n_vals)){
  
  x = rnorm(mean = 0, sd = 1, n = n_vals[j])
  y = rnorm(mean = 1.5, sd = 2, n = n_vals[j])
  
  include = c(rep(TRUE, n_vals[j]), rep(FALSE, n_vals[j]))
  
  base_test_stat = t.test(x=x, y=y, alternative = "less")$statistic
  
  x_y = c(x, y)
  
  perm_test_stats = numeric(N)
  
  for (i in 1 : N){
    x_y_shuffled = sample(x_y)
    temp_x = x_y_shuffled[include]
    temp_y = x_y_shuffled[!include]
    perm_test_stats[i] = t.test(x = temp_x, y = temp_y, alternative = "less")$statistic
  }
  
  reject = (mean(perm_test_stats < base_test_stat) < alpha)

  hist(perm_test_stats, freq = FALSE, main = n_vals[j])
  abline(v = base_test_stat, col = "red")
}

