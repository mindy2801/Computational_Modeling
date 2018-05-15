data {
  int<lower=1> T;
  int<lower=0, upper=1> gamble[T];
  real cert[T];
  real<lower=0> gain[T];
  real<lower=0> loss[T];  // absolute loss amount
}
transformed data {
}
parameters {
  real<lower=0, upper=2> rho;
  real<lower=0, upper=5> lambda;
  real<lower=0, upper=10> tau;
}
transformed parameters {
}
model {
  // ra_prospect: Original model in Soko-Hessner et al 2009 PNAS
  // for a single subject
  rho    ~ uniform(0, 2);
  lambda ~ uniform(0, 5);
  tau    ~ uniform(0, 10);
  
  for (t in 1:T) {
    
    real evSafe;    
    real evGamble;
    real pGamble;

    // loss[t]=absolute amount of loss (pre-converted in R)
    evSafe   = pow(cert[t], rho);
    evGamble = 0.5 * (pow(gain[t], rho) - lambda * pow(loss[t], rho));
    pGamble  = inv_logit(tau * (evGamble - evSafe));
    gamble[t] ~ bernoulli(pGamble);
  }
}
generated quantities{
  // For posterior predictive check
  real y_pred[T];

  for (t in 1:T) {
    real evSafe;
    real evGamble;
    real pGamble;

    evSafe     = pow(cert[t], rho);
    evGamble   = 0.5 * (pow(gain[t], rho) - lambda * pow(loss[t], rho));
    pGamble    = inv_logit(tau * (evGamble - evSafe));

    // generate posterior prediction for current trial
    y_pred[t] = bernoulli_rng(pGamble);
  }
}
