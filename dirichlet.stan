data { 
  int K;  // number of catagories
  int N;  // number of sites 
  int y[N, K];  // data matrix
  vector<lower=0>[K] alpha; //normalized prior location parameters for dirichlet distribution
  real<lower=0> logsigma; //prior variance parameter for dirichlet distribution

} 
parameters { 
  real<lower=0> sigma;   //posterior variance parameter for dirichlet distribution
  simplex[K] theta[N];   //posterior estimates of data
} 
transformed parameters { 
  vector<lower=0>[K] alphaprime;  //normalized posterior location parameters for dirichlet distribution 
  alphaprime <- alpha/sigma;  //calculation of alpha prime 
}
model { 
  sigma ~ lognormal(0, logsigma);  
  for (n in 1:N) { 
    theta[n] ~ dirichlet(alphaprime); 
    y[n] ~ multinomial(theta[n]); 
  } 
} 