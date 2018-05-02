data {
  int<lower=1> T;
  int<lower=0> k[T];
  int<lower=1> n[T];
  
}

parameters {
  real theta;

  
}

model {
  theta ~ beta(2, 2);
  k ~ binomial(n, theta);

}
