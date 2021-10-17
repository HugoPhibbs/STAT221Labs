x_sample <- c(1.5, 3.0, 2.2, 2.5, 4.1, 3.7)
alpha <- 0.05

# Doing the test
one_sided_test = t.test(x_sample, alternative = "greater", mu = 2)
print(one_sided_test$p.value < alpha)

# We can reject the null hypothesis since p value for this test is 0.044,
# Which is outside our accepted range of 0.05
two_sided_test = t.test(x_sample, alternative = "two.sided", mu = 2)
print(two_sided_test$p.value < alpha)

