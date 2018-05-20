# hw5_model1_exec.R
# Programmed by Woo-Young Ahn (wahn55@snu.ac.kr), May 2018

#rm(list=ls())  # remove all variables

library(rstan)

# set working directory where all the files exist
setwd("~/Dropbox/Teaching/SNU/2018_Computational_modeling/HW_for_instructor/HW5/")

# read the data file
dat = read.table("simul_data_hw5_model1.txt", header=T, sep="\t")

allSubjs = unique(dat$subjID)  # all subject IDs
N = length(allSubjs)      # number of subjects
T = table(dat$subjID)[1]  # number of trials per subject 

choice  <- array(-1, c(N, T))
outcome <- array(0, c(N, T))

for (i in 1:N) {
  curSubj = allSubjs[i]
  tmp     = subset(dat, subjID == curSubj)
  choice[i, 1:T] <- tmp$choice
  outcome[i, 1:T] <- tmp$outcome
}

dataList <- list(
  N       = N,
  T       = T,
  Tsubj   = rep(T, N),
  choice  = choice,
  outcome = outcome
)

# run!
output = stan("hw5_model1.stan", data = dataList, 
              iter = 2000, warmup=1000, chains=2, cores=2)

# traceplot
traceplot(output)

# print summary
print(output)

# extract Stan fit object (parameters)
parameters <- rstan::extract(output)

alpha_mean = apply(parameters$alpha, 2, mean)
alpha_sd = apply(parameters$alpha, 2, sd)
beta_mean = apply(parameters$beta, 2, mean)
beta_sd = apply(parameters$beta, 2, sd)

## 1.2a
source("HDIofMCMC.R")
source("multiplot.R")
library(dplyr)
library(ggplot2)

### Group parameters: mu_p, sigma
colnames(parameters$mu_p) <- c("alpha","beta")
colnames(parameters$sigma) <- c("alpha","beta")

mu_p <- parameters$mu_p %>% reshape2::melt() %>% rename(parameter=Var2)
mu_p_HDI <- mu_p %>% group_by(parameter) %>% summarise(mean=mean(value),
                                                       HDI1=HDIofMCMC(value)[1], 
                                                       HDI2=HDIofMCMC(value)[2])

sigma <- parameters$sigma %>% reshape2::melt() %>% rename(parameter=Var2)
sigma_HDI <- sigma %>% group_by(parameter) %>% summarise(mean=mean(value),
                                                         HDI1=HDIofMCMC(value)[1], 
                                                         HDI2=HDIofMCMC(value)[2])


g_mu <- ggplot(mu_p, aes(value, fill=parameter)) + geom_histogram(bins = 50) + 
  facet_wrap(~parameter, nrow=2) + 
  geom_vline(data=mu_p_HDI, aes(xintercept=mean), 
             linetype="dashed", size=1) +
  geom_errorbarh(data=mu_p_HDI, aes(y=0, x=mean, xmin=HDI1, xmax=HDI2), 
                 height=20, size=1) +
  ylab(label="") +
  theme(axis.text.y=element_blank(),  axis.ticks.y=element_blank(), 
        axis.title.x=element_blank(),
        legend.position="none")

g_sigma <- ggplot(sigma, aes(value, fill=parameter)) + geom_histogram(bins = 50) + 
  facet_wrap(~parameter, nrow=2) + 
  geom_vline(data=sigma_HDI, aes(xintercept=mean), 
             linetype="dashed", size=1) +
  geom_errorbarh(data=sigma_HDI, aes(y=0, x=mean, xmin=HDI1, xmax=HDI2), 
                 height=20, size=1) +
  ylab(label="") +
  theme(axis.text.y=element_blank(),  axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position="none")

multiplot(g_mu, g_sigma, cols=2)

### Individual parameters: alpha, beta
colnames(parameters$alpha) <- paste("subj",1:N, sep="")
colnames(parameters$beta) <- paste("subj",1:N, sep="")

alpha <- parameters$alpha %>% reshape2::melt() %>% rename(subject=Var2)
alpha_HDI <- alpha %>% group_by(subject) %>% summarise(mean=mean(value),
                                                       sd  =sd(value),
                                                         HDI1=HDIofMCMC(value)[1], 
                                                         HDI2=HDIofMCMC(value)[2])

beta <- parameters$beta %>% reshape2::melt() %>% rename(subject=Var2)
beta_HDI <- beta %>% group_by(subject) %>% summarise(mean=mean(value),
                                                       sd  =sd(value),
                                                       HDI1=HDIofMCMC(value)[1], 
                                                       HDI2=HDIofMCMC(value)[2])

i_alpha <- ggplot(alpha, aes(value, fill=subject)) + geom_histogram(bins = 50) + 
  facet_wrap(~subject, ncol=1) + 
  geom_vline(data=alpha_HDI, aes(xintercept=mean), 
             linetype="dashed", size=1) +
  geom_errorbarh(data=alpha_HDI, aes(y=0, x=mean, xmin=HDI1, xmax=HDI2), 
                 height=20, size=1) +
  ylab(label="") +
  theme(axis.text.y=element_blank(),  axis.ticks.y=element_blank(), 
        axis.title.x=element_blank(),
        legend.position="none")

i_beta <- ggplot(beta, aes(value, fill=subject)) + geom_histogram(bins = 50) + 
  facet_wrap(~subject, ncol=1) + 
  geom_vline(data=beta_HDI, aes(xintercept=mean), 
             linetype="dashed", size=1) +
  geom_errorbarh(data=beta_HDI, aes(y=0, x=mean, xmin=HDI1, xmax=HDI2), 
                 height=20, size=1) +
  ylab(label="") +
  theme(axis.text.y=element_blank(),  axis.ticks.y=element_blank(), 
        axis.title.x=element_blank(),
        legend.position="none")



## 1.2b
### x: true parameters, y: estimated parameters and add 1sd error bars
set.seed(08826)
true_pars <- data.frame(alpha = rnorm(N, 0.20, 0.08),
                         beta = rnorm(N, 2.00, 0.70),
                         subjID  = 1:N) #true
true_pars <- reshape2::melt(true_pars, id="subjID")

estm_pars <- data.frame(alpha = alpha_HDI$mean, 
                        beta = beta_HDI$mean, 
                        subjID = 1:N)
estm_pars <- reshape2::melt(estm_pars, id="subjID")

pars_compare <- merge(true_pars, estm_pars, by=c("subjID","variable")) %>% 
  arrange(variable, subjID) %>% rename(true=value.x, estm=value.y, parameter=variable)
  
ggplot(pars_compare, aes(x=true, y=estm)) + geom_point() + 
  facet_wrap(~parameter, scales = "free") +
  geom_abline(aes(intercept=0, slope=1, colour="red"))


