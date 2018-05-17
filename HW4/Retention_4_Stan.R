rm(list=ls()) 
library(rstan)

model <- "

data { 
int ns;
int nt;
int k[ns - 1,nt - 1];
int t[nt];
int n;
}
parameters {
vector<lower=0,upper=1>[ns] alpha;
real<lower=0,upper=1> beta;
real<lower=0,upper=1> alphamu;
real<lower=0,upper=1> betamu;
real<lower=.001> alphalambda;
real<lower=.001> betalambda;
} 
transformed parameters {
matrix<lower=0,upper=1>[ns,nt] theta;
real<lower=0> alphasigma;
real<lower=0> betasigma;

alphasigma = inv_sqrt(alphalambda);
betasigma = inv_sqrt(betalambda);

// Retention Rate At Each Lag For Each Subject Decays Exponentially

for (i in 1:ns)
for (j in 1:nt)
theta[i,j] = fmin(1.0, exp(-alpha[i] * t[j]) + beta);

}
model {
// Priors For Group Distributions
alphamu ~ beta(1, 1);  // can be removed
betamu ~ beta(1, 1);  // can be removed
alphalambda ~ gamma(.001, .001)T[.001,];
betalambda ~ gamma(.001, .001)T[.001,];

// Parameters For Each Subject Drawn From Gaussian Group Distributions
beta ~ normal(betamu, betasigma)T[0,1];
for (i in 1:ns)  {
alpha[i] ~ normal(alphamu, alphasigma)T[0,1];
}

// Observed Data

for (i in 1:(ns - 1))
for (j in 1:(nt - 1))
k[i,j] ~ binomial(n, theta[i,j]);

}
generated quantities {
int<lower=0,upper=n> predk[ns,nt];
real log_lik[ns];

// Predicted Data
for (i in 1:(ns-1)){
  log_lik[i] = 0;
  for (j in 1:(nt-1)){
    log_lik[i] = log_lik[i] + binomial_lpmf(k[i,j]|n, theta[i,j]);

    predk[i,j] = binomial_rng(n, theta[i,j]);
    }
  }


}
"



t     <- c(1, 2, 4, 7, 12, 21, 35, 59, 99, 200)
nt    <- length(t)
slist <- 1:4
ns    <- length(slist)


k1 <- matrix(c(18, 18, 16, 13, 9, 6, 4, 4, 4, NA,
               17, 13,  9,  6, 4, 4, 4, 4, 4, NA,
               14, 10,  6,  4, 4, 4, 4, 4, 4, NA,
               NA, NA, NA, NA,NA,NA,NA,NA,NA, NA), nrow=ns, ncol=nt, byrow=T)

k <- k1[1:(ns - 1), 1:(nt - 1)]   # Excluding NAs (for Stan solution)
n <- 18
data <- list(k=k, n=n, t=t, ns=ns, nt=nt) # To be passed on to Stan



myinits <- list(
  list(alphamu=.5, alphalambda=1, betamu=.5, betalambda=1, 
       alpha=rep(.5, ns), beta=.1))

samples <- stan(model_code=model,   
                data=data, 
                init=myinits,  # If not specified, gives random inits
                iter=10000, 
                chains=1, 
                thin=1,
                ,warmup = 100,  # Stands for burn-in; Default = iter/2
                
                # seed = 123  # Setting seed; Default is random seed
                
)

# LOOIC
library(loo)
log_lik <- extract_log_lik(samples)
looic <- loo(log_lik)
print(looic)

# Graph
library(ggplot2)
library(reshape2)
library(dplyr)
source("HDIofMCMC.R")

parameters <- rstan::extract(samples)

# arrange dataframe
ls(parameters)
names <- paste("sbj", 1:ns)
colnames(parameters$alpha) <- names
colnames(parameters$theta) <- names

# plot posteriors for group parameters
#alphamu, alphasigma, betamu, betasigma
group <- data.frame(value=c(parameters$alphasigma, parameters$alphamu,
                          parameters$betasigma, parameters$betamu),
                    index=rep(c("alphasigma","alphamu", "betasigma","betamu"), each=nrow(parameters$alphasigma)))


group_HDI <- group %>% group_by(index) %>% 
  summarise(mean=mean(value),HDI1=HDIofMCMC(value)[1], HDI2=HDIofMCMC(value)[2])

g <- ggplot(group, aes(value)) + geom_histogram(bins = 50) + 
  facet_wrap(~index, scale="free_x") + 
  geom_vline(data=group_HDI, aes(xintercept=mean), 
             linetype="dashed", size=1) +
  geom_errorbarh(data=group_HDI, aes(y=0, x=mean, xmin=HDI1, xmax=HDI2), 
                 height=20, size=1) +
  ylab(label="")

# plot posteriors for individual parameters
# alpha, beta, theta
individual_alpha <- data.frame(parameters$alpha,
                         index=rep(c("alpha"), each=nrow(parameters$alpha)))
individual_alpha <- melt(individual_alpha, id="index")

individual_beta <- data.frame(value=parameters$beta,
                              index=rep(c("beta"), each=nrow(parameters$beta)))

alpha_HDI <- individual_alpha %>% group_by(index, variable) %>% 
  summarise(mean=mean(value),HDI1=HDIofMCMC(value)[1], HDI2=HDIofMCMC(value)[2])
beta_HDI <- individual_beta %>%  
  summarise(mean=mean(value),HDI1=HDIofMCMC(value)[1], HDI2=HDIofMCMC(value)[2])

i_alpha <- ggplot(individual_alpha, aes(value, fill=variable)) + geom_histogram(bins = 50) + 
  facet_wrap(~index+variable, scale="free_x", nrow=3) + 
  geom_vline(data=alpha_HDI, aes(xintercept=mean), 
             linetype="dashed", size=1) +
  geom_errorbarh(data=alpha_HDI, aes(y=0, x=mean, xmin=HDI1, xmax=HDI2), 
                 height=20, size=1) +
  ylab(label="")

i_beta <- ggplot(individual_beta, aes(value)) + geom_histogram(bins = 50) + 
  geom_vline(data=beta_HDI, aes(xintercept=mean), 
             linetype="dashed", size=1) +
  geom_errorbarh(data=beta_HDI, aes(y=0, x=mean, xmin=HDI1, xmax=HDI2), 
                 height=20, size=1) +
  ylab(label="")

# posterior distributions
g #group parameters
i_alpha #alpha
i_beta #beta
