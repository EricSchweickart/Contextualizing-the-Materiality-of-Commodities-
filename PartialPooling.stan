data {
  int<lower=0> nObs;        
  int<lower=0> nSites;        
  int<lower=0> site[nObs];  
  int<lower=0> N[nObs];       
  int<lower=0> obs[nObs];     
  real<lower=0> alpha;        
  real<lower=0> beta;         
  real<lower=0> sigma;        
}  

parameters {
  real<lower=0, upper=1> omega;   
  real<lower=2> kappa;              
  vector<lower=0, upper=1>[nSites] theta;
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
  theta ~ beta(a,b);                      

  for(n in 1:nObs)
    obs[n] ~ binomial(N[n], theta[site[n]]);  
}

