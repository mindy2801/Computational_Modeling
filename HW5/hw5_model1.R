# hw5_model1_exec.R
# Programmed by Woo-Young Ahn (wahn55@snu.ac.kr), May 2018

#rm(list=ls())  # remove all variables

library(rstan)

# read the data file
dat = read.table("simul_data_hw5_model1.1.txt", header=T, sep="\t")

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

## 1.2a
source("HDIofMCMC.R")
source("multiplot.R")
library(dplyr)
library(ggplot2)
library(reshape2)

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


g_mu <- ggplot(mu_p, aes(value)) + geom_density() + 
  facet_wrap(~parameter, nrow=2) 

d <- ggplot_build(g_mu)$data[[1]] 
d <- d %>% mutate(parameter=rep(c("alpha","beta"), each=nrow(d)/2)) %>% select(x,y,parameter) %>% 
  merge(mu_p_HDI, by="parameter") %>% group_by(parameter) %>% 
  filter(x <= HDI2 & x >= HDI1)

g_mu <- g_mu + geom_area(data = d, 
                 aes(x=x, y=y), fill="#FF9999", colour="black", group="parameter") + 
  geom_vline(data=mu_p_HDI, aes(xintercept=mean)) + 
  xlab("") + theme_bw()
 

g_sigma <- ggplot(sigma, aes(value)) + geom_density() + 
  facet_wrap(~parameter, nrow=2) 

d <- ggplot_build(g_sigma)$data[[1]] 
d <- d %>% mutate(parameter=rep(c("alpha","beta"), each=nrow(d)/2)) %>% 
  select(x,y,parameter) %>% 
  merge(sigma_HDI, by="parameter") %>% group_by(parameter) %>% 
  filter(x <= HDI2 & x >= HDI1)

g_sigma <- g_sigma + geom_area(data = d, 
                 aes(x=x, y=y), fill="#FF9999", colour="black", group="parameter") + 
  geom_vline(data=sigma_HDI, aes(xintercept=mean)) + 
  xlab("") + theme_bw()



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



i_alpha <- ggplot(alpha, aes(value)) + geom_density() + 
  facet_wrap(~subject, ncol=2) 

d <- ggplot_build(i_alpha)$data[[1]] 
d <- d %>% mutate(subject=rep(paste("subj",1:N, sep=""), each=nrow(d)/N)) %>% 
  select(x,y,subject) %>% 
  merge(alpha_HDI, by="subject") %>% group_by(subject) %>% 
  filter(x <= HDI2 & x >= HDI1)

i_alpha <-i_alpha + geom_area(data = d, 
                               aes(x=x, y=y), fill="#FF9999", colour="black", group="subject") + 
  geom_vline(data=alpha_HDI, aes(xintercept=mean)) + 
  xlab("") + theme_bw()

i_beta <- ggplot(beta, aes(value)) + geom_density() + 
  facet_wrap(~subject, ncol=2) 

d <- ggplot_build(i_beta)$data[[1]] 
d <- d %>% mutate(subject=rep(paste("subj",1:N, sep=""), each=nrow(d)/N)) %>% 
  select(x,y,subject) %>% 
  merge(beta_HDI, by="subject") %>% group_by(subject) %>% 
  filter(x <= HDI2 & x >= HDI1)

i_beta <-i_beta + geom_area(data = d, 
                              aes(x=x, y=y), fill="#FF9999", colour="black", group="subject") + 
  geom_vline(data=beta_HDI, aes(xintercept=mean)) + 
  xlab("") + theme_bw()


multiplot(i_alpha, i_beta, cols=2)

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
  
c <- ggplot(pars_compare, aes(x=true, y=estm)) + geom_point() + 
  facet_wrap(~parameter, scales = "free") +
  geom_abline(aes(intercept=0, slope=1, colour="red")) +
  geom_errorbar(aes(ymin=estm-sd, ymax=estm+sd))

