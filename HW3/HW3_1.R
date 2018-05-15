rm(list=ls())  # remove all variables 
library(rstan)

# read the data file
T <- 6
k <- c(11, 18, 15, 39, 32, 43)
N <- c(20, 25, 30, 55, 45, 57)

dataList <- list(
  T = T,
  k = k,
  N = N
)

# run!
output = stan("HW3_1.stan", data = dataList, iter = 2000, warmup=1000, chains=2, cores=2)

# traceplot
traceplot(output)

# print summary
print(output)

# extract Stan fit object (parameters)
parameters <- rstan::extract(output)

# plot posteriors 
hist(parameters$theta)

save(output, parameters, file="HW3_1.RData")
