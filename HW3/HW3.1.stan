data {
  int<lower=1> T;
  int<lower=0, upper=1> gamble[T];
  real cert[T];
  real<lower=0> gain[T];
  real<lower=0> loss[T];  // absolute loss amount
}

parameters {
  real theta;

  
}

model {
  theta ~ beta(2, 2);

}
