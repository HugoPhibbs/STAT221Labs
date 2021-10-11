source("euclidean_distance.R")


check_in_circle <- function(radius, point, centre = c(0, 0)) {
    return(euclidean_distance(centre, point) <= radius)
}