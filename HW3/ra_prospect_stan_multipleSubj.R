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

# use first subject only

dataList <- list(
  T       = T,
  N       = N,
  gain    = matrix(dat$gain, nrow=N, ncol=T, byrow=T), #matrix[N,T]
  loss    = matrix(abs(dat$loss), nrow=N, ncol=T, byrow=T), # absolute value
  cert    = matrix(dat$cert, nrow=N, ncol=T, byrow=T),
  gamble  = matrix(dat$gamble, nrow=N, ncol=T, byrow=T)
)

# run!
output = stan("ra_prospect_multipleSubj.stan", data = dataList,
              iter = 1000, warmup=500, chains=2, cores=2)

### load existing output
load("ra_prospect_multipleSubj.RData")

# traceplot
traceplot(output, pars="rho")
traceplot(output, pars="lambda")
traceplot(output, pars="tau")

# print summary
print(output)

# extract Stan fit object (parameters)
parameters <- rstan::extract(output)
names <- paste("sbj", allSubjs)
colnames(parameters$rho) <- names
colnames(parameters$lambda) <- names
colnames(parameters$tau) <- names
colnames(parameters$y_pred) <- names

# 95% HDI of rho
HDIofMCMC(parameters$rho, credMass = 0.95)


# 2.1 plot posteriors 
library(ggplot2)
library(gridExtra)
combined <- reshape2::melt(parameters)
combined <- combined[combined$L1!="lp__", -1]

hist_q2_1 <- ggplot(data = combined[combined$L1!="y_pred",], mapping = aes(x = value)) + 
  geom_histogram(bins = 50) + facet_wrap(~L1+Var2, scales = 'free', nrow=3) +
  theme_classic() +
  theme(axis.title.y=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
hist_q2_1

# 2.2 Calculate posterior means of 5 subjects
library(dplyr)
posterior_mean <- combined %>% select("Var2", "value", "L1") %>%  filter(L1 %in% c("rho", "lambda","tau")) %>% 
  group_by(Var2, L1) %>% summarise(mean=mean(value)) %>% 
  rename(subj=Var2, parameter=L1, mean=mean)

# 2.3 Compare MLE estimates
library(reshape2)
load("mle_par.RData")
posterior_mean <- posterior_mean %>% as.data.frame() %>% 
  arrange(subj, parameter)
mle_par <- mle_par %>% melt() %>% rename(subj=Var1, parameter=Var2, mean=value) 
mle_par$parameter <- as.character(mle_par$parameter)
mle_par <- arrange(mle_par, subj, parameter)

xy_compare <- posterior_mean %>% select(subj, parameter, mean) %>% rename(bayes=mean) %>%
  mutate(mle=mle_par$mean)

ggplot(xy_compare, aes(x=bayes, y=mle)) + geom_point() + facet_wrap(~parameter) +
  geom_abline(aes(intercept=0, slope=1, colour="red"))


