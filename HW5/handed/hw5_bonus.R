rm(list=ls())

# 4.1 
dat <- read.table("simul_data_hw5_bonus1.txt", header=T, sep="\t")
library(rstan)

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
output = stan("hw5_bonus.stan", data = dataList, 
              iter = 2000, warmup=1000, chains=2, cores=2)

# traceplot
traceplot(output)

# print summary
print(output)

# extract Stan fit object (parameters)
load("hw5_bonus1.RData")
parameters <- rstan::extract(output)

## 4.1
source("HDIofMCMC.R")
source("multiplot.R")
library(dplyr)
library(ggplot2)
library(reshape2)

### Group parameters: mu_p, sigma
colnames(parameters$mu_p) <- c("alpha_pos","alpha_neg","beta")
colnames(parameters$sigma) <- c("alpha_pos","alpha_neg","beta")

mu_p <- parameters$mu_p %>% reshape2::melt() %>% rename(parameter=Var2)
mu_p_HDI <- mu_p %>% group_by(parameter) %>% summarise(mean=mean(value),
                                                       HDI1=HDIofMCMC(value)[1], 
                                                       HDI2=HDIofMCMC(value)[2])

sigma <- parameters$sigma %>% reshape2::melt() %>% rename(parameter=Var2)
sigma_HDI <- sigma %>% group_by(parameter) %>% summarise(mean=mean(value),
                                                         HDI1=HDIofMCMC(value)[1], 
                                                         HDI2=HDIofMCMC(value)[2])


g4_mu <- ggplot(mu_p, aes(value)) + geom_density() + 
  facet_wrap(~parameter, nrow=3) 

d <- ggplot_build(g4_mu)$data[[1]] 
d <- d %>% mutate(parameter=rep(c("alpha_pos","alpha_neg","beta"), each=nrow(d)/3)) %>% select(x,y,parameter) %>% 
  merge(mu_p_HDI, by="parameter") %>% group_by(parameter) %>% 
  filter(x <= HDI2 & x >= HDI1)

g4_mu <- g4_mu + geom_area(data = d, 
                           aes(x=x, y=y), fill="#FF9999", colour="black", group="parameter") + 
  geom_vline(data=mu_p_HDI, aes(xintercept=mean)) + 
  xlab("") + theme_bw()


g4_sigma <- ggplot(sigma, aes(value)) + geom_density() + 
  facet_wrap(~parameter, nrow=3) 

d <- ggplot_build(g4_sigma)$data[[1]] 
d <- d %>% mutate(parameter=rep(c("alpha_pos","alpha_neg","beta"), each=nrow(d)/3)) %>% 
  select(x,y,parameter) %>% 
  merge(sigma_HDI, by="parameter") %>% group_by(parameter) %>% 
  filter(x <= HDI2 & x >= HDI1)

g4_sigma <- g4_sigma + geom_area(data = d, 
                                 aes(x=x, y=y), fill="#FF9999", colour="black", group="parameter") + 
  geom_vline(data=sigma_HDI, aes(xintercept=mean)) + 
  xlab("") + theme_bw()



multiplot(g4_mu, g4_sigma, cols=2)

### Individual parameters: alpha, beta
colnames(parameters$alpha_pos) <- paste("subj",1:N, sep="")
colnames(parameters$alpha_neg) <- paste("subj",1:N, sep="")
colnames(parameters$beta) <- paste("subj",1:N, sep="")

alpha_pos <- parameters$alpha_pos %>% reshape2::melt() %>% rename(subject=Var2)
alpha_pos_HDI <- alpha_pos %>% group_by(subject) %>% summarise(mean=mean(value),
                                                       sd  =sd(value),
                                                       HDI1=HDIofMCMC(value)[1], 
                                                       HDI2=HDIofMCMC(value)[2])

alpha_neg <- parameters$alpha_neg %>% reshape2::melt() %>% rename(subject=Var2)
alpha_neg_HDI <- alpha_neg %>% group_by(subject) %>% summarise(mean=mean(value),
                                                               sd  =sd(value),
                                                               HDI1=HDIofMCMC(value)[1], 
                                                               HDI2=HDIofMCMC(value)[2])


beta <- parameters$beta %>% reshape2::melt() %>% rename(subject=Var2)
beta_HDI <- beta %>% group_by(subject) %>% summarise(mean=mean(value),
                                                     sd  =sd(value),
                                                     HDI1=HDIofMCMC(value)[1], 
                                                     HDI2=HDIofMCMC(value)[2])



i4_alpha_pos <- ggplot(alpha_pos, aes(value)) + geom_density() + 
  facet_wrap(~subject, ncol=6) 

d <- ggplot_build(i4_alpha_pos)$data[[1]] 
d <- d %>% mutate(subject=rep(paste("subj",1:N, sep=""), each=nrow(d)/N)) %>% 
  select(x,y,subject) %>% 
  merge(alpha_pos_HDI, by="subject") %>% group_by(subject) %>% 
  filter(x <= HDI2 & x >= HDI1)

i4_alpha_pos <-i4_alpha_pos + geom_area(data = d, 
                                aes(x=x, y=y), fill="#FF9999", colour="black", group="subject") + 
  geom_vline(data=alpha_pos_HDI, aes(xintercept=mean)) + 
  xlab("") + theme_bw()

i4_alpha_neg <- ggplot(alpha_neg, aes(value)) + geom_density() + 
  facet_wrap(~subject, ncol=6) 

d <- ggplot_build(i4_alpha_neg)$data[[1]] 
d <- d %>% mutate(subject=rep(paste("subj",1:N, sep=""), each=nrow(d)/N)) %>% 
  select(x,y,subject) %>% 
  merge(alpha_neg_HDI, by="subject") %>% group_by(subject) %>% 
  filter(x <= HDI2 & x >= HDI1)

i4_alpha_neg <-i4_alpha_neg + geom_area(data = d, 
                                        aes(x=x, y=y), fill="#FF9999", colour="black", group="subject") + 
  geom_vline(data=alpha_neg_HDI, aes(xintercept=mean)) + 
  xlab("") + theme_bw()


i4_beta <- ggplot(beta, aes(value)) + geom_density() + 
  facet_wrap(~subject, ncol=6) 

d <- ggplot_build(i4_beta)$data[[1]] 
d <- d %>% mutate(subject=rep(paste("subj",1:N, sep=""), each=nrow(d)/N)) %>% 
  select(x,y,subject) %>% 
  merge(beta_HDI, by="subject") %>% group_by(subject) %>% 
  filter(x <= HDI2 & x >= HDI1)

i4_beta <-i4_beta + geom_area(data = d, 
                              aes(x=x, y=y), fill="#FF9999", colour="black", group="subject") + 
  geom_vline(data=beta_HDI, aes(xintercept=mean)) + 
  xlab("") + theme_bw()



## 4.2
### x: true parameters, y: estimated parameters and add 1sd error bars
set.seed(08826)
true_pars = data.frame(alpha_pos = rnorm(N, 0.20, 0.08),
                        alpha_neg = rnorm(N, 0.30, 0.10),
                        beta = rnorm(N, 2.00, 0.70),
                        subjID  = 1:N) 
true_pars <- melt(true_pars, id="subjID")

estm_pars <- data.frame(alpha_pos = alpha_pos_HDI$mean,
                        alpha_neg = alpha_neg_HDI$mean,
                        beta = beta_HDI$mean, 
                        subjID = 1:N)
estm_pars <- melt(estm_pars, id="subjID")
estm_pars <- estm_pars %>% mutate(sd=c(alpha_pos_HDI$sd, alpha_neg_HDI$sd, 
                                       beta_HDI$sd))

pars_compare <- merge(true_pars, estm_pars, by=c("subjID","variable")) %>% 
  arrange(variable, subjID) %>% rename(true=value.x, estm=value.y, parameter=variable)

c4.1 <- ggplot(pars_compare, aes(x=true, y=estm)) + geom_point() + 
  facet_wrap(~parameter, scales = "free") +
  geom_abline(aes(intercept=0, slope=1, colour="red")) +
  geom_errorbar(aes(ymin=estm-sd, ymax=estm+sd))


#4.3.
loc <- which(!(ls() %in% c("i4_alpha_neg", "i4_alpha_pos", "i4_beta", 
                  "g4_mu", "g4_sigma", "c4.1")))
rm(list=ls()[loc])


source("HDIofMCMC.R")
source("multiplot.R")
dat <- read.table("simul_data_hw5_bonus2.txt", header=T, sep="\t")

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
output = stan("hw5_bonus.stan", data = dataList, 
              iter = 2000, warmup=1000, chains=2, cores=2)


# extract Stan fit object (parameters)
load("hw5_bonus2.RData")
parameters <- rstan::extract(output)

### Group parameters: mu_p, sigma
colnames(parameters$mu_p) <- c("alpha_pos","alpha_neg","beta")
colnames(parameters$sigma) <- c("alpha_pos","alpha_neg","beta")

mu_p <- parameters$mu_p %>% reshape2::melt() %>% rename(parameter=Var2)
mu_p_HDI <- mu_p %>% group_by(parameter) %>% summarise(mean=mean(value),
                                                       HDI1=HDIofMCMC(value)[1], 
                                                       HDI2=HDIofMCMC(value)[2])

sigma <- parameters$sigma %>% reshape2::melt() %>% rename(parameter=Var2)
sigma_HDI <- sigma %>% group_by(parameter) %>% summarise(mean=mean(value),
                                                         HDI1=HDIofMCMC(value)[1], 
                                                         HDI2=HDIofMCMC(value)[2])


### Individual parameters: alpha, beta
colnames(parameters$alpha_pos) <- paste("subj",1:N, sep="")
colnames(parameters$alpha_neg) <- paste("subj",1:N, sep="")
colnames(parameters$beta) <- paste("subj",1:N, sep="")

alpha_pos <- parameters$alpha_pos %>% reshape2::melt() %>% rename(subject=Var2)
alpha_pos_HDI <- alpha_pos %>% group_by(subject) %>% summarise(mean=mean(value),
                                                               sd  =sd(value),
                                                               HDI1=HDIofMCMC(value)[1], 
                                                               HDI2=HDIofMCMC(value)[2])

alpha_neg <- parameters$alpha_neg %>% reshape2::melt() %>% rename(subject=Var2)
alpha_neg_HDI <- alpha_neg %>% group_by(subject) %>% summarise(mean=mean(value),
                                                               sd  =sd(value),
                                                               HDI1=HDIofMCMC(value)[1], 
                                                               HDI2=HDIofMCMC(value)[2])


beta <- parameters$beta %>% reshape2::melt() %>% rename(subject=Var2)
beta_HDI <- beta %>% group_by(subject) %>% summarise(mean=mean(value),
                                                     sd  =sd(value),
                                                     HDI1=HDIofMCMC(value)[1], 
                                                     HDI2=HDIofMCMC(value)[2])

set.seed(08826)
true_pars = data.frame(alpha_pos = rnorm(N, 0.20, 0.08),
                        alpha_neg = rnorm(N, 0.30, 0.10),
                        beta = rnorm(N, 2.00, 0.70),
                        subjID  = 1:N) 
true_pars <- melt(true_pars, id="subjID")

estm_pars <- data.frame(alpha_pos = alpha_pos_HDI$mean,
                        alpha_neg = alpha_neg_HDI$mean,
                        beta = beta_HDI$mean, 
                        subjID = 1:N)
estm_pars <- melt(estm_pars, id="subjID")
estm_pars <- estm_pars %>% mutate(sd=c(alpha_pos_HDI$sd, alpha_neg_HDI$sd, 
                                       beta_HDI$sd))

pars_compare <- merge(true_pars, estm_pars, by=c("subjID","variable")) %>% 
  arrange(variable, subjID) %>% rename(true=value.x, estm=value.y, parameter=variable)

c4.2 <- ggplot(pars_compare, aes(x=true, y=estm)) + geom_point() + 
  facet_wrap(~parameter, scales = "free") +
  geom_abline(aes(intercept=0, slope=1, colour="red")) +
  geom_errorbar(aes(ymin=estm-sd, ymax=estm+sd))


save(c4.1, c4.2, g4_mu, g4_sigma, i4_alpha_pos, i4_alpha_neg, i4_beta, file="graph4.RData")
