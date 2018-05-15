data {
  int ns;
  int nt;
  int k[ns - 1,nt - 1];
  int t[nt];
  int n;
}
transformed data {
}
parameters {
  vector<lower=0,upper=1>[ns] alpha;
  real<lower=0,upper=1> beta;
  real<lower=0,upper=1> alphamu;
  real<lower=.001> alphalambda;
} 
transformed parameters {
  matrix<lower=0,upper=1>[ns,nt] theta;
  real<lower=0> alphasigma;
  
  alphasigma = inv_sqrt(alphalambda);
  
  // Retention Rate At Each Lag For Each Subject Decays Exponentially
  for (i in 1:ns)
    for (j in 1:nt)
      theta[i,j] = fmin(1.0, exp(-alpha[i] * t[j]) + beta);
}
model {
  // Priors For Group Distributions
  alphamu ~ beta(1, 1);  // can be removed
  beta ~ beta(1, 1);  // can be removed
  alphalambda ~ gamma(.001, .001)T[.001,];

  // Parameters For Each Subject Drawn From Gaussian Group Distributions
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
  
  // Predicted Data
  for (i in 1:ns)
    for (j in 1:nt)
      predk[i,j] = binomial_rng(n, theta[i,j]);
}
