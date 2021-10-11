# Given the vector:
x <- c(a = 6, b = 2, c = -1, d = 7, e = 0)
# Write R code to

# calculate the sum of all elements
# calculate the number of elements
# calculate the arithmetic mean of all elements
# extract the element with name b
# calculate the sum of the first three elements
# calculate the sum of all elements that are larger than 2
# extract the smallest value
# extract all values which are either smaller than 0 or larger than 3

# Summing elements
summed <- sum(x)

# Finding number of elements
length_of_x <- length(x)

# Finding mean of elements
mean <- mean(x)

# Find element with name b
element <- x[b]

# Find sum of first 3 elements
sum_first_3 <- sum(x[1:3])
# OR
sum_first_3 <- 0
for (i in 1:3) {
    sum_first_3 <- sum_first_3 + x[i]
}

# Find sum of all elements larger than 2
# is there a filter function I could be using?
sum_larger_than_2 <- 0
for (j in 1:length(x)) {
    if (x[j] > 2) {
        sum_larger_than_2 <- sum_larger_than_2 + x[j]
    }
}

# Find smallest value
min <- min(x)

# Extract all elements that are smaller than 0 or larger than 3
result <- c()
for (k in 1:length(x)) {
    curr <- x[k]
    if (curr < 0 | curr > 3) {
        result[k] <- curr
    }
}