library(datasets)
data(faithful)
help(faithful)

eruption_times <- faithful[, 1]

# Question 1
breaks <- seq(0, 6, 0.2)
hist_graph <- hist(eruption_times, breaks = breaks, freq = FALSE)
lines(hist_graph$mid, hist_graph$density, col = "purple", lwd = 2)
