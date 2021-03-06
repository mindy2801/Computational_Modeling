
#######################################################
## model_select.R                                    ##
## PSYCH 7695, Spring 2017                           ##
## Maximum Likelihood Estimation (Myung, JMP, 2003)  ##
## By Yun Tang, Psychology, OSU                      ##
##                                                   ##
## Main Program                                      ##
## Code Written on 12/18/2009                        ##
##                                                   ##
## Modified by Joonsuk Park on Jan 28 2015           ##
## Modified by Jay Myung in Feb 2017                 ##
## Modified by Woo-Young Ahn in March 2018           ##
#######################################################

# Loading the (minus) log-likelihood functions
# Please modify the path according to the actual location of the file "MLE_LSE.R"
# e.g., setwd("/Users/youngahn/this-course/")

rm(list=ls())  # clear workspace
graphics.off() # close all figures

set.seed(08826)  # set a seed number for replication
source("MLE.R")   # source MLE.R code

##########################
## General Setup        ##
## Data and Parameters  ##
##########################
n_total <- 50 # sample size
t_int <- c(0.5, 1, 2, 4, 8, 12, 16, 18) # time interval values
n_corr <- c(44, 34, 27, 26, 19, 17, 20, 11) # number of correct responses
p_corr <- n_corr/n_total # proportion correct

# Generate random uniform numbers between 0 and 1 to use as initials for the optim procedure


param1_init <- runif(1)
param2_init <- runif(2)
param3_init <- runif(3)
init <- list(param1_init, param2_init, param1_init, param2_init, param3_init, param1_init, param2_init)

param_pow1_low <- c(0); param_pow1_up <- c(3);
param_pow2_low <- c(0, 0); param_pow2_up <- c(1, 3);  # lower and upper bounds of POW2 model (0<a<1, 0<b<3)
param_exp1_low <- c(0); param_exp1_up <- c(3);
param_exp2_low <- c(0, 0); param_exp2_up <- c(1, 3);  # lower and upper bounds of EXP2 model (0<a<1, 0<b<3)
param_expow_low <- c(0, 0, -Inf); param_expow_up <- c(1, Inf, 3);
param_hyp1_low <- c(0); param_hyp1_up <- c(1);
param_hyp2_low <- c(0, 0); param_hyp2_up <- c(1, 1);

param_low <- list(param_pow1_low, param_pow2_low, param_exp1_low, param_exp2_low, param_expow_low, param_hyp1_low, param_hyp2_low)
param_up <- list(param_pow1_up, param_pow2_up, param_exp1_up, param_exp2_up, param_expow_up, param_hyp1_up, param_hyp2_up)

##########################
## MLE                  ##
##########################

# Call general purpose optimization rountine

mle_model <- c(mle_pow1, mle_pow2, mle_exp1, mle_exp2, mle_expow, mle_hyp1, mle_hyp2)

mle_model_optim <- list()

for (model in 1:7){
  mle_model_optim[[model]] <- optim(init[[model]], mle_model[[model]], method="L-BFGS-B", lower=param_low[[model]], upper=param_up[[model]], int=t_int, n=n_total, x=n_corr)
  
}

#mle_model_pow1 <- optim(param1_init, mle_pow1, method="L-BFGS-B", lower=param_pow1_low, upper=param_pow1_up, int=t_int, n=n_total, x=n_corr)


# Try many different inits to escape from the local maxima
for (i in 1:100) {
  param1_init <- runif(1); param2_init <- runif(2); param3_init <- runif(3); 
  init <- list(param1_init, param2_init, param1_init, param2_init, param3_init, param1_init, param2_init)
  temp_model <- list()
  
  for (model in 1:7){
    
    temp_model[[model]] <- optim(init[[model]], mle_model[[model]], method="L-BFGS-B", lower=param_low[[model]], upper=param_up[[model]], int=t_int, n=n_total, x=n_corr)
    
  
  
  # Replace the results if the latest optimization yields better result
  if(temp_model[[model]]$value < mle_model_optim[[model]]$value) mle_model_optim[[model]] <- temp_model[[model]]  

  }
}

# Save the MLE parameter estimates
param_model <- list()

for (model in 1:7){
  param_model[[model]] <- mle_model_optim[[model]]$par
  
}


# number of parameters (k_modelName)
n_par <- c(1,2,1,2,3,1,2)

# number of data points (N)
N = length(p_corr) 

# Compute AIC = -2*log(lik) + 2*K
AIC <- function(n_par=c(), mle_model_optim=list()){
  values <- c()
  
  for (model in 1:7){
    values[model] <- 2*mle_model_optim[[model]]$value + 2*n_par[model]
  }
  values
}

all_AIC <- AIC(n_par, mle_model_optim)

# Compute BIC = -2*log(lik) + K*log(N)
BIC <- function(n_par=c(), N, mle_model_optim=list()){
  values <- c()
  
  for (model in 1:7){
    values[model] <- 2*mle_model_optim[[model]]$value + n_par[model]*log(N)
  }
  values
}

all_BIC <- BIC(n_par, N, mle_model_optim)

# Generate summary

names = c("POW1","POW2","EXP1", "EXP2", "EXPOW", "HYP1", "HYP2")

modelcomp_summary = data.frame(Models = names, AIC = all_AIC, BIC = all_BIC)
print(modelcomp_summary)