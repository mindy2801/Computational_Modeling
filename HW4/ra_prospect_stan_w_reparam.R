# ra_prospect_stan_singleSubj.R
# Programmed by Woo-Young Ahn (wahn55@snu.ac.kr), Apr 2018

rm(list=ls())  # remove all variables 
library(rstan)

# source HDIofMCMC.R to calculate HDI
source("HDIofMCMC.R") 

# read the data file
dat = read.table("ra_exampleData.txt", header=T, sep="\t")

allSubjs = unique(dat$subjID)  # all subject IDs
N = length(allSubjs)      # number of subjects
T = table(dat$subjID)[1]  # number of trials per subject (=140)
numIter = 100             # number of iterations to find global minimum values
numPars = 3               # number of parameters


dataList <- list(
  T       = T,
  N       = N,
  Tsubj   = table(dat$subjID),
  gain    = matrix(dat$gain, nrow=N, ncol=T, byrow=T), #matrix[N,T]
  loss    = matrix(abs(dat$loss), nrow=N, ncol=T, byrow=T), # absolute value
  cert    = matrix(dat$cert, nrow=N, ncol=T, byrow=T),
  gamble  = matrix(dat$gamble, nrow=N, ncol=T, byrow=T)
)

# run!
output = stan("ra_prospect_w_reparam.stan", data = dataList,
              iter = 1000, warmup=500, chains=2, cores=2)

### load existing output
load("ra_prospect_w_reparam.RData")

# traceplot
traceplot(output, pars="rho")
traceplot(output, pars="lambda")
traceplot(output, pars="tau")

# print summary
print(output)

# extract Stan fit object (parameters)
parameters <- rstan::extract(output)
ls(parameters)
names <- paste("sbj", allSubjs)
colnames(parameters$rho) <- names
colnames(parameters$lambda) <- names
colnames(parameters$tau) <- names
colnames(parameters$y_pred) <- names

# 95% HDI of rho
HDIofMCMC(parameters$rho, credMass = 0.95)


# 2.2.1 plot posteriors for group parameters
#mu_p sigma
library(ggplot2)
library(reshape2)
library(dplyr)

colnames(parameters$sigma) <- c("rho","lambda","tau")
colnames(parameters$mu_p) <- c("rho","lambda","tau")

sigma <- melt(parameters$sigma); colnames(sigma)[2] <- "parameter"
mu_p <- melt(parameters$mu_p); colnames(mu_p)[2] <- "parameter"

group <- data.frame(rbind(sigma, mu_p), index=rep(c("sigma","mu_p"), each=3000))
group_HDI <- group %>% group_by(index, parameter) %>% 
  summarise(mean=mean(value),HDI1=HDIofMCMC(value)[1], HDI2=HDIofMCMC(value)[2])

ggplot(group, aes(value, fill=parameter)) + geom_histogram(bins = 50) + 
  facet_wrap(~index+parameter, scale="free_x") + 
  geom_vline(data=group_HDI, aes(xintercept=mean), 
             linetype="dashed", size=1) +
  geom_errorbarh(data=group_HDI, aes(y=0, x=mean, xmin=HDI1, xmax=HDI2), 
                height=20, size=1) +
  ylab(label="")


# 2.2.2 plot posteriors for individual parameters
# rho lambda tau
colnames(parameters$rho) <- paste("sbj_",allSubjs, sep="")
colnames(parameters$lambda) <- paste("sbj_",allSubjs, sep="")
colnames(parameters$tau) <- paste("sbj_",allSubjs, sep="")

rho <- melt(parameters$rho); colnames(rho)[2] <- "subject"
lambda <- melt(parameters$lambda); colnames(lambda)[2] <- "subject"
tau <- melt(parameters$tau); colnames(tau)[2] <- "subject"

individual <- data.frame(rbind(rho,lambda,tau), index=rep(c("rho","lambda","tau"), each=5000))
individual_HDI <- individual %>% group_by(index, subject) %>% 
  summarise(mean=mean(value),HDI1=HDIofMCMC(value)[1], HDI2=HDIofMCMC(value)[2])

ggplot(individual, aes(value, fill=subject)) + geom_histogram(bins = 50) + 
  facet_wrap(~index+subject, scale="free_x", nrow=3) + 
  geom_vline(data=individual_HDI, aes(xintercept=mean), 
             linetype="dashed", size=1) +
  geom_errorbarh(data=individual_HDI, aes(y=0, x=mean, xmin=HDI1, xmax=HDI2), 
                 height=20, size=1) +
  ylab(label="")


