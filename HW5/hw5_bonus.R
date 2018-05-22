# generate data for bonus question
rm(list=ls())

# Simulation parameters
seed <- 08826    # do not change the seed number!
num_subjs  <- 30 # number of subjects
num_trials <- 100 # number of trials per subject
pr_correct_option1 <- 0.5  # reward probability in option 1
pr_correct_option2 <- 0.8  # reward probability in option 2

# Set seed
set.seed(seed)   # always set a seed number for this homework!

# True parameters 
simul_pars = data.frame(alpha_pos = rnorm(num_subjs, 0.20, 0.08),
                        alpha_neg = rnorm(num_subjs, 0.30, 0.10),
                        beta = rnorm(num_subjs, 2.00, 0.70),
                        subjID  = 1:num_subjs) 

# For storing simulated choice data for all subjects
all_data <- NULL

for (i in 1:num_subjs) {
  # Individual-level (i.e. per subject) parameter values
  alpha_pos <- simul_pars$alpha_pos[i]
  alpha_neg <- simul_pars$alpha_neg[i]
  beta <- simul_pars$beta[i]
  
  # geneate payoff structure for each subject
  # Defaults for the two options 
  # option 1: 50% win (+1), 50% loss (-1)
  # option 2: 80% win (+1), 20% loss (-1)
  payoff_option1 = rbinom(size=1, n = num_trials, prob = pr_correct_option1)
  payoff_option2 = rbinom(size=1, n = num_trials, prob = pr_correct_option2)
  
  # Replace 0 with -1
  payoff_option1[payoff_option1 == 0] = -1   # if 0 --> replace it with -1
  payoff_option2[payoff_option2 == 0] = -1   # if 0 --> replace it with -1
  payoff_both = data.frame(payoff_option1, payoff_option2)
  
  # For storing simulated data for current subject
  # subjID = subject ID
  # trial = trial number
  # choice = choice made on each trial (1 or 2)
  # outcome = outcome reveived on each trial (1 or -1)
  tmp_data = data.frame( subjID=NULL, trial=NULL, choice=NULL, outcome=NULL)
  
  # initialize some variables
  sv = c(0, 0)  # stimulus value of two options
  
  for (t in 1:num_trials)  {
    # Prob of choosing option 2
    prob_choose2 = 1 / (1 + exp(beta * (sv[1] - sv[2])))  # exploration/exploitation parameter is set to 1
    
    # choice
    choice = rbinom(size=1, n = 1, prob = prob_choose2 )
    choice = choice + 1  # 0 or 1 --> 1 (option 1) or 2 (option 2)
    
    # outcome
    outcome = payoff_both[t, choice]
    
    # after receiving outcome (feedback), update sv[t+1]
    # prediction error (PE)
    PE = outcome - sv[choice]
    
    # update stimulus value (sv) of the chosen option
    if (PE >= 0){
      sv[choice] = sv[choice] + alpha_pos * (outcome - sv[choice] )
    } else{
      sv[choice] = sv[choice] + alpha_neg * (outcome - sv[choice] )
    }

    
    # append simulated task/response to subject data
    tmp_data[t, "subjID"] = i
    tmp_data[t, "trial"] = t
    tmp_data[t, "choice"] = choice
    tmp_data[t, "outcome"] = outcome
  } # end of t loop
  # Append current subject with all subjects' data
  all_data = rbind(all_data, tmp_data)
}

dat <- all_dat

#
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

