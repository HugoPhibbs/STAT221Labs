# Our X sample
x = c(1.5, 3.0, 2.2, 2.5, 4.1, 3.7)

# Level of significance
alpha = 0.05

# Question 1

# Performing a t test (one sided)
one_sided_test = t.test(x, alternative = "greater", mu = 2)
one_sided_reject = (one_sided_test$p.value < alpha)
print(one_sided_reject)

# Reject the null hypothesis

# Question 2

# Performing a t test (two sided)
two_sided_test = t.test(x, alternative = "two.sided", mu = 2)
two_sided_reject = (two_sided_test$p.value < (alpha / 2))
print(two_sided_reject)

# Accept the null hypothesis

# Did i do rejection part right?
