library(datasets)
data(faithful)
help(faithful)

eruption_times <- faithful[, 1]

# Question 1
hist(eruption_times, xlim = c(1, 6), breaks = 10)

# Question 2
breaks <- ceiling(1 + log2(length(eruption_times)))
# Hist already uses sturges rule by default

# Question 3
breaks <- seq(0, 6, 0.2)
hist(eruption_times, breaks = breaks)
# I think that the best width choice is 0.2 Shows off the curve of the graph
# And doesnt hide as much detail as larger width sizes
