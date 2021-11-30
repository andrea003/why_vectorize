##### MULTIPLYING TWO VECTORS #####

# vectorized
vector_vectorized <- function(n) {
    x <- 1:n
    y <- x * x 
    return(y)
}

# for loop
vector_for_loop <- function(n) {
    x <- 1:n
    y <- vector("numeric", length = n)
    
    for (i in 1:n) {
        y[i] <- x[i] * x[i]
    }
    return(y)
}

# load the microbenchmark package
library(microbenchmark)

# microbenchmark to compare the two functions
compare_multiplication <- microbenchmark(vector_vectorized(100),
                                         vector_for_loop(100),
                                         times = 100)


##### GENERATE RANDOM NUMBERS #####

# runif() function
generate_runif <- function(n) {
    random_numbers <- runif(n = n, min = 0, max = 1)
}

# for loop
generate_forloop <- function(n) {
    random_numbers <- vector("numeric", length = n)
    for (i in 1:n) {
        random_numbers[i] <- runif(n = 1, min = 0, max = 1)
    }
}

# compare generating 1,000,000 random numbers 
compare_generate <- microbenchmark(generate_runif(1000000),
                                   generate_forloop(1000000),
                                   times = 10)



##### MONTE CARLO SIMULATION TO ESTIMATE PI #####

# estimating pi VECTORIZED
pi_vectorized <- function(seed = 28, iterations = 1000000){
    set.seed(seed)
    
    x <- runif(n = iterations, min = -1, max = 1)
    y <- runif(n = iterations, min = -1, max = 1)
    
    sum_sq_xy <- sqrt(x^2 + y^2) 
    index_within_circle <- which(sum_sq_xy <= 1)
    points_within_circle <- length(index_within_circle)
    
    pi_est <- 4 * points_within_circle / iterations
    return(pi_est)
}

# estimating pi using a FOR LOOP
pi_for_loop <- function(seed = 28, iterations = 1000000) {
    points_within_circle <- 0
    set.seed(seed)
    
    for (i in 1:iterations) {
        x <- runif(n = 1, min = -1, max = 1)
        y <- runif(n = 1, min = -1, max = 1)
        if(sqrt(x^2 + y^2) <= 1) {
            points_within_circle <- points_within_circle + 1
        } 
    } 
    return(4 * points_within_circle / iterations) 
}


# run the benchmark, specifying 'times = 10' will make the test run each function 10 times
compare_pi <- microbenchmark(pi_vectorized(iterations = 1000000),
                             pi_for_loop(iterations = 1000000),
                             times = 10)