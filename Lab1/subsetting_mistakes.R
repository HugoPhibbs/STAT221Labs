# x[x = 7]
# x[-1:3]
# x[x == 2 | 7]

x <- c(a = 6, b = 2, c = -1, d = 7, e = 0)

# Should be ?
# x[x = 7] ??
x[x = 2 | 7] # == is used for equality
x[1:3] # Can't use negatives with slicing, except for with 0