# ra_prospect_stan_singleSubj.R
# Programmed by Woo-Young Ahn (wahn55@snu.ac.kr), Apr 2018

rm(list=ls())  # remove all variables 

library(rstan)
source("HDIofMCMC.R") 

# read the data file
dat = read.table("ra_exampleData.txt", header=T, sep="\t")

allSubjs = unique(dat$subjID)  # all subject IDs
N = length(allSubjs)      # number of subjects
T = table(dat$subjID)[1]  # number of trials per subject (=140)
numIter = 100             # number of iterations to find global minimum values
numPars = 3               # number of parameters

#2.1 Calculate posterior distributions for 5 subjects
par <- array(NA, c(2000, 4, N))
for(i in 1:N){
  tmpData = subset(dat, subjID==i)
  dataList <- list(
    T       = T,
    gain    = tmpData$gain,
    loss    = abs(tmpData$loss),   # absolute value
    cert    = tmpData$cert,
    gamble  = tmpData$gamble)
    
  output = stan("ra_prospect_singleSubj.stan", data = dataList, pars = c("rho", "lambda", "tau"),
                  iter = 2000, warmup=1000, chains=2, cores=2)
    
  parameters[,,N] <- extract(output)##
  
}

# traceplot
traceplot(output)

# print summary
print(output)

# extract Stan fit object (parameters)
parameters <- rstan::extract(output)

# plot posteriors 
hist(parameters$rho)
hist(parameters$lambda)
hist(parameters$tau)

# 95% HDI of rho
HDIofMCMC(parameters$rho, credMass = 0.95)




#2.2 Calculate posterior means for 5 subjects
#2.3 Compare MLE estimated and posteriorm means
