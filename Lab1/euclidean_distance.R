euclidean_distance <- function(point_1, point2) {
    x1 <- point_1[1]
    x2 <- point2[1]

    y1 <- point_1[2]
    y2 <- point2[2]

    distance <- sqrt((x1 - x2)^2 + (y1 - y2)^2)

    return(distance)
}