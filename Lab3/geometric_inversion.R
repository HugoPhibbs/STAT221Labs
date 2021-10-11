inv_geo_cdf = function(u, p){
    
    x = (log(1-u))/(log(1-p)) - 1

    return(x)
}


create_x_sample = function(u_sample, p){
    
    sample_size = length(u_sample)
    x_sample = numeric(sample_size)

    for (i in seq(sample_size)){
        x_sample[i] = inv_geo_cdf(u_sample[i], p)
    }

    return(x_sample)
}

p = 0.3;

u_sample = runif(10000, 0, 1)

x_sample = create_x_sample(u_sample, p)

hist(x_sample, xlim = c(-5, 30), breaks = 1000, freq = FALSE)

x_sample


# Sample with the true distribution

x_sample_true = seq(0, 30)

y_sample = dgeom(x_sample_true, p)

lines(x_sample_true, y_sample, col="red")
