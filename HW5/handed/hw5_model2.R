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

alpha <- parameters$alpha %>% melt() %>% rename(subject=Var2)
alpha_HDI <- alpha %>% group_by(subject) %>% summarise(mean=mean(value),
                                                       sd  =sd(value),
                                                       HDI1=HDIofMCMC(value)[1], 
                                                       HDI2=HDIofMCMC(value)[2])

beta <- parameters$beta %>% melt() %>% rename(subject=Var2)
beta_HDI <- beta %>% group_by(subject) %>% summarise(mean=mean(value),
                                                     sd  =sd(value),
                                                     HDI1=HDIofMCMC(value)[1], 
                                                     HDI2=HDIofMCMC(value)[2])


i2_alpha <- ggplot(alpha, aes(value)) + geom_density() + 
  facet_wrap(~subject, ncol=6, scale="free_y") 

d <- ggplot_build(i2_alpha)$data[[1]] 
d <- d %>% mutate(subject=rep(paste("subj",1:N, sep=""), each=nrow(d)/N)) %>% 
  select(x,y,subject) %>% 
  merge(alpha_HDI, by="subject") %>% group_by(subject) %>% 
  filter(x <= HDI2 & x >= HDI1)

i2_alpha <-i2_alpha + geom_area(data = d, 
                              aes(x=x, y=y), fill="#FF9999", colour="black", group="subject") + 
  geom_vline(data=alpha_HDI, aes(xintercept=mean)) + 
  xlab("") + theme_bw()

i2_beta <- ggplot(beta, aes(value)) + geom_density() + 
  facet_wrap(~subject, ncol=6, scales="free_y") 

d <- ggplot_build(i2_beta)$data[[1]] 
d <- d %>% mutate(subject=rep(paste("subj",1:N, sep=""), each=nrow(d)/N)) %>% 
  select(x,y,subject) %>% 
  merge(beta_HDI, by="subject") %>% group_by(subject) %>% 
  filter(x <= HDI2 & x >= HDI1)

i2_beta <-i2_beta + geom_area(data = d, 
                            aes(x=x, y=y), fill="#FF9999", colour="black", group="subject") + 
  geom_vline(data=beta_HDI, aes(xintercept=mean)) + 
  xlab("") + theme_bw()


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


save(c2,i2_alpha, i2_beta, file="graph2.RData")
