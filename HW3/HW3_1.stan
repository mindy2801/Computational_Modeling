data {
  int<lower=1> T;
  int<lower=0> k[T];
  int<lower=1> N[T];
  
}

parameters {
  real<lower=0, upper=1> theta;
  
}
model {
  theta ~ beta(2,2);
  k ~ binomial(N, theta);
  
}
