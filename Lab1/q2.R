euclidean_distance <- function(point_1, point_2) {
    x1 <- point_1[1]
    x2 <- point_2[1]

    y1 <- point_1[2]
    y2 <- point_2[2]

    distance <- sqrt((x1 - x2)^2 + (y1 - y2)^2)
    return(distance)
}

check_in_circle <- function(radius, point, centre = c(0, 0)) {
    return(euclidean_distance(centre, point) <= radius)
}

print_distances <- function(x_vals, y_vals) {
    for (i in 1:length(x)) {
        point <- c(x_vals[i], y_vals[i])
        print(check_in_circle(3, point))
    }
}

print_distances(
    x <- c(1, 4, -1, 3),
    y <- c(0, 2, 2, -3)
)

