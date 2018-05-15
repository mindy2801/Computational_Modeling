data {
  int<lower=1> T; //total trial number
  int<lower=1> N; //subject number
  int<lower=0, upper=1> gamble[N, T];
  real cert[N, T];
  real<lower=0> gain[N, T];
  real<lower=0> loss[N, T];  // absolute loss amount
}
transformed data {
}
parameters {
  real<lower=0, upper=2> rho[N];
  real<lower=0, upper=5> lambda[N];
  real<lower=0, upper=10> tau[N];
}
transformed parameters {
}
model {
  // ra_prospect: Original model in Soko-Hessner et al 2009 PNAS
  // for a single subject
  rho[N]    ~ uniform(0, 2);
  lambda[N] ~ uniform(0, 5);
  tau[N]    ~ uniform(0, 10);
  
  for (n in 1:N) {
    for (t in 1:T) {
      
      real evSafe;    
      real evGamble;
      real pGamble;
  
      // loss[t]=absolute amount of loss (pre-converted in R)
      evSafe   = pow(cert[n, t], rho[n]);
      evGamble = 0.5 * (pow(gain[n, t], rho[n]) - lambda[n] * pow(loss[n, t], rho[n]));
      pGamble  = inv_logit(tau[n] * (evGamble - evSafe));
      gamble[n, t] ~ bernoulli(pGamble);
    }
    
  }
}
generated quantities{
  // For posterior predictive check
  real y_pred[N, T];
  
  for(n in 1:N) {
    for (t in 1:T) {
      real evSafe;
      real evGamble;
      real pGamble;
  
      evSafe     = pow(cert[n, t], rho[n]);
      evGamble   = 0.5 * (pow(gain[n, t], rho[n]) - lambda[n] * pow(loss[n, t], rho[n]));
      pGamble    = inv_logit(tau[n] * (evGamble - evSafe));
  
      // generate posterior prediction for current trial
      y_pred[n, t] = bernoulli_rng(pGamble);
    }
  }
}
