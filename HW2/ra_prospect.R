ra_prospect <- function(param, T=140){

  
  sum_minusLL = 0  # sum of minus log likelihood. Initialize
  for (t in 1:T) {
    #evSafe: expected value of a certain (safe) option
    #evGamble: expected value of a risky option (gamble)
    #pGamble   # probability of choosing a gamble on each trial
    # free parameters: rho, tau, lambda
    rho <- param[1]; tau <- param[2]; lambda <- param[3]
    
    evSafe   = cert[t]^rho
    evGamble = 0.5*(gain[t]^rho - lambda*abs(loss[t])^rho) 
    pGamble  = 1 / (1 + exp(tau*(evSafe - evGamble)))
    pGamble  = pGamble * 0.9998 + 0.0001  # to make its range between 0.0001 and 0.9999
    tmp_minusLL   = -log(pGamble)*gamble[t] - log(1-pGamble)*(1-gamble[t])  # LL of trial t
    sum_minusLL   = sum_minusLL + tmp_minusLL
  }
  sum_minusLL
} 

ra_noLA <- function(param, T=140){

  sum_minusLL = 0  # sum of minus log likelihood. Initialize
  
  for (t in 1:T) {
    #evSafe: expected value of a certain (safe) option
    #evGamble: expected value of a risky option (gamble)
    #pGamble   # probability of choosing a gamble on each trial
    # free parameters: rho, tau, lambda
    rho <- param[1]; tau <- param[2]
    
    evSafe   = cert[t]^rho
    evGamble = 0.5*(gain[t]^rho - abs(loss[t])^rho) 
    pGamble  = 1 / (1 + exp(tau*(evSafe - evGamble)))
    pGamble  = pGamble * 0.9998 + 0.0001  # to make its range between 0.0001 and 0.9999
    tmp_minusLL   = -log(pGamble)*gamble[t] - log(1-pGamble)*(1-gamble[t])  # LL of trial t
    sum_minusLL   = sum_minusLL + tmp_minusLL
  }
  sum_minusLL

}
