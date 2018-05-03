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
all_output <- list()
for(i in allSubjs){
  tmpData = subset(dat, subjID==i)
  dataList <- list(
    T       = T,
    gain    = tmpData$gain,
    loss    = abs(tmpData$loss),   # absolute value
    cert    = tmpData$cert,
    gamble  = tmpData$gamble)
    
  output = stan("ra_prospect_singleSubj.stan", data = dataList, pars = c("rho", "lambda", "tau"),
                  iter = 2000, warmup=1000, chains=2, cores=2)
    
  all_output[[i]] <- output
  
}

par_mean <- matrix(nrow=N, ncol=3)


# extract Stan fit object (parameters)
for(i in allSubjs){
  parameters <- rstan::extract(all_output[[i]])

# plot posteriors 
png(file=paste("subject",i,".png",sep=""))  
par(mfrow=c(1,3))

hist(parameters$rho, main="", xlab="Rho")
HDI <- HDIofMCMC(parameters$rho, credMass = 0.95)
abline(v=HDI, col="red", lty=2)

hist(parameters$lambda, main=paste("Subject",i), xlab="Lambda")
HDI <- HDIofMCMC(parameters$lambda, credMass = 0.95)
abline(v=HDI, col="red", lty=2)

hist(parameters$tau, main="", xlab="Tau")
HDI <- HDIofMCMC(parameters$tau, credMass = 0.95)
abline(v=HDI, col="red", lty=2)

dev.off()

#2.2 Calculate posterior means for 5 subjects
par_mean[which(allSubjs==i),] <- round(sapply(parameters,mean)[1:3],3)

}

par_mean <- rbind(par_mean,colMeans(par_mean))
colnames(par_mean) <- c("Rho", "Lambda", "Tau")
rownames(par_mean) <- c(paste("Subject", allSubjs), "Total")

#2.3 Compare MLE estimated and posteriorm means by plotting
load("mle_estimates.RData")
mle_par_mean <- rbind(mle_par_mean,colMeans(mle_par_mean))
colnames(mle_par_mean) <- c("Rho", "Lambda", "Tau")
rownames(mle_par_mean) <- c(paste("Subject", allSubjs), "Total")
mle_par_mean <- round(mle_par_mean,3)

png(file="estimates_comparison.png", width=800)  
par(mfrow=c(1,3))
plot(mle_par_mean[,1], par_mean[,1], main="", xlab="MLE mean rho", ylab="Posterior mean rho")
plot(mle_par_mean[,2], par_mean[,2], main="", xlab="MLE mean lambda", ylab="Posterior mean lambda")
plot(mle_par_mean[,3], par_mean[,3], main="", xlab="MLE mean tau", ylab="Posterior mean tau")
dev.off()

