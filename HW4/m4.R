t     <- c(1, 2, 4, 7, 12, 21, 35, 59, 99, 200)
nt    <- length(t)
slist <- 1:4
ns    <- length(slist)

k1 <- matrix(c(18, 18, 16, 13, 9, 6, 4, 4, 4, NA,
               17, 13,  9,  6, 4, 4, 4, 4, 4, NA,
               14, 10,  6,  4, 4, 4, 4, 4, 4, NA,
               NA, NA, NA, NA,NA,NA,NA,NA,NA, NA), nrow=ns, ncol=nt, byrow=T)

k <- k1[1:(ns - 1), 1:(nt - 1)]   # Excluding NAs (for Stan solution)

n <- 18

data <- list(k=k, n=n, t=t, ns=ns, nt=nt) # To be passed on to Stan

myinits <- list(
  list(alphamu=.5, alphalambda=1, betamu=.5, betalambda=1, 
       alpha=rep(.5, ns), beta=.1))

parameters <- c("alpha", "beta", "predk")  # Parameters to be monitored

# For a detailed description type "?rstan".
samples <- stan("Retention_3.stan",   
                data=data, 
                init=myinits,  # If not specified, gives random inits
                pars=parameters,
                iter=1000, 
                chains=1, 
                warmup = 10
                )#,  # Stands for burn-in; Default = iter/2
                # seed = 123  # Setting seed; Default is random seed

# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.

