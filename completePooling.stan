data {
  int<lower=0> N;             // No. sampled newts
  int<lower=0> obs;           // No. infected newts
  real<lower=0> alpha;        // priors on theta
  real<lower=0> beta;         // priors on theta
  real<lower=0> sigma;
}  

parameters {
  real<lower=0, upper=1> theta; // grand infect prob.
  real<lower=0, upper=1> omega;   
  real<lower=2> kappa; 
}

transformed parameters {
  real<lower=0> a;
  real<lower=0> b;
  a <- omega * (kappa - 2) +1;
  b <- (1 - omega) * (kappa - 2) + 1;
}

model {
  omega ~ beta(alpha,beta);               
  kappa ~ normal(2, sigma); 
  theta ~ beta(a, b);  // prior for thetas
  obs ~ binomial(N, theta);  
}

