# Q1 Sample cauchy pdf using inverse sampling

inv_cauchy_cdf = function(u){

    x = tan(pi*(u-(1/2)))

    return(x)
}

create_x_sample = function(u_sample){
    
    sample_size = length(u_sample)
    x_sample = numeric(sample_size)

    for (i in seq(sample_size)){
        x_sample[i] = inv_cauchy_cdf(u_sample[i])
    }

    return(x_sample)
}

u_sample = runif(10000, 0, 1)

x_sample = create_x_sample(u_sample)

hist(x_sample, freq=FALSE, xlim = c(-5, 5), breaks = 30000)

x_sample

# Sample with the actual distribution

x_sample_true = seq(-10, 10)
y_sample = dcauchy(x_sample_true)
lines(x_sample_true, y_sample, col = "red") # Use lines to add to an existing plot

# Plots look almost identical!!! YAS 
