data {
  int<lower=0> nObs;          // Number of buttons
  int<lower=0> nSites;        // Number of sites
  int<lower=0> site[nObs];    // Site button was found on
  vector<lower=0>[nObs] diam; // Average diameter of button
  real<lower=0> muMean;       // mean of prior mu
  real<lower=0> muSD;         // SD of prior mu
  real<lower=0> sigmaSD;      // scale for sigma
}  

parameters {
  vector<lower=0> [nSites] mu;
  real<lower=0> sigma;
}

model {
  mu ~ normal(muMean, muSD);
  sigma ~ cauchy(0, sigmaSD);
  
   for(n in 1:nObs)
    diam [n] ~ normal(mu [site[n]], sigma);
}

