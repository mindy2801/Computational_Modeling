library(rstan)

dat <- list(T=6, 
            k=c(11,18,15,39,32,43), 
            n=c(20,25,30,55,45,57))

fit <- stan("HW3.1.stan", data = dat, pars = "theta",
                     iter = 2000, warmup=1000, chains=2, cores=2)

parameters <- extract(fit)

#2a. Posterior distribution of theta
hist(parameters$theta, xlim=c(0,1), breaks=20, main="Posterior distribution of theta")

#2b. 95% HDI for theta
source("HDIofMCMC.R") 
HDI <- HDIofMCMC(parameters$theta, credMass = 0.95)
abline(v=HDI, col="red", lty=2)
text(0.6,1, round(HDI[1],3), cex=2); text(0.75, 1, round(HDI[2],3), cex=2)
