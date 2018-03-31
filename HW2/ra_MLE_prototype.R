data <- read.table("ra_exampleData.txt", header=T); attach(data)
N = length(unique(data$subjID))  # number of subjects
T = length(data[data$subjID==2,"outcome"]) # number of trials per subject

# for a single subject=2
sum_minusLL = 0  # sum of minus log likelihood. Initialize
init <- runif(3)  
rho <- init[1]; tau <- init[2]; lambda <- init[3]

for (t in 1:T) {
  #evSafe: expected value of a certain (safe) option
  #evGamble: expected value of a risky option (gamble)
  #pGamble   # probability of choosing a gamble on each trial
  # free parameters: rho, tau, lambda
  
  evSafe   = cert[t]^rho
  evGamble = 0.5*(gain[t]^rho - lambda*abs(loss[t])^rho) 
  pGamble  = 1 / (1 + exp(tau*(evSafe - evGamble)))
  pGamble  = pGamble * 0.9998 + 0.0001  # to make its range between 0.0001 and 0.9999
  tmp_minusLL   = -log(pGamble)*gamble[t] - log(1-pGamble)*(1-gamble[t])  # LL of trial t
  sum_minusLL   = sum_minusLL + tmp_minusLL
}

optim(sum_minusLL, method="L-BFGS-B", lower=param_low[[model]], upper=param_up[[model]], int=t_int, n=n_total, x=n_corr)


###
AIC <- function(n_par=c(), mle_model_optim=list()){
  values <- c()
  
  for (model in 1:7){
    values[model] <- 2*mle_model_optim[[model]]$value + 2*n_par[model]
  }
  values
}

# Compute BIC = -2*log(lik) + K*log(N)
BIC <- function(n_par=c(), N, mle_model_optim=list()){
  values <- c()
  
  for (model in 1:7){
    values[model] <- 2*mle_model_optim[[model]]$value + n_par[model]*log(N)
  }
  values
}
