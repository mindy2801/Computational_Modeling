rm(list=ls())
library(rstan)

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
output = stan("hw5_model2.stan", data = dataList, 
              iter = 2000, warmup=1000, chains=2, cores=2)

# traceplot
traceplot(output)

# print summary
print(output)

# extract Stan fit object (parameters)
parameters <- rstan::extract(output)


## 1.2a
source("HDIofMCMC.R")
source("multiplot.R")
library(dplyr)
library(ggplot2)
library(reshape2)


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

i2_alpha <- ggplot(alpha, aes(value, fill=subject)) + geom_histogram(bins = 50) + 
  facet_wrap(~subject, ncol=1) + 
  geom_vline(data=alpha_HDI, aes(xintercept=mean), 
             linetype="dashed", size=1) +
  geom_errorbarh(data=alpha_HDI, aes(y=0, x=mean, xmin=HDI1, xmax=HDI2), 
                 height=20, size=1) +
  ylab(label="") +
  theme(axis.text.y=element_blank(),  axis.ticks.y=element_blank(), 
        axis.title.x=element_blank(),
        legend.position="none")

i2_beta <- ggplot(beta, aes(value, fill=subject)) + geom_histogram(bins = 50) + 
  facet_wrap(~subject, ncol=1) + 
  geom_vline(data=beta_HDI, aes(xintercept=mean), 
             linetype="dashed", size=1) +
  geom_errorbarh(data=beta_HDI, aes(y=0, x=mean, xmin=HDI1, xmax=HDI2), 
                 height=20, size=1) +
  ylab(label="") +
  theme(axis.text.y=element_blank(),  axis.ticks.y=element_blank(), 
        axis.title.x=element_blank(),
        legend.position="none")

multiplot(i2_alpha, i2_beta, cols=2)

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
estm_pars <- estm_pars %>% mutate(sd=c(alpha_HDI$sd, beta_HDI$sd))

pars_compare <- merge(true_pars, estm_pars, by=c("subjID","variable")) %>% 
  arrange(variable, subjID) %>% rename(true=value.x, estm=value.y, parameter=variable)

c2 <- ggplot(pars_compare, aes(x=true, y=estm)) + geom_point() + 
  facet_wrap(~parameter, scales = "free") +
  geom_abline(aes(intercept=0, slope=1, colour="red")) +
  geom_errorbar(aes(ymin=estm-sd, ymax=estm+sd))

