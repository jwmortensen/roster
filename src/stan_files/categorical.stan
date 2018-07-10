data {
  int<lower=0> N; // number of data points
  int<lower=0> D; // number of dimensions for mixture model
  int<lower=0> K; // number of mixture components
  vector[D] X[N]; // data for mixture assignment
  int<lower=0, upper=4> y[N]; // categorical response (categories are 1,2,3);
}

parameters {
  vector[D] mu[K];
  cholesky_factor_corr[D] L[K];
}

transformed parameters {
  simplex[K] probs[N]; // probability of membership for each mixture component
  vector[K] dens[N];
  for (n in 1:N) {
    for (k in 1:K) {
      dens[n][k] = multi_normal_cholesky_lpdf(X[n] | mu[k], L[k]);
    }
    probs[n] = softmax(dens[n]);
  }
}

model {
  for(k in 1:K){
    mu[k] ~ normal(0,3);
    L[k] ~ lkj_corr_cholesky(4);
  }
  for (n in 1:N)
    y ~ categorical(probs[n]);
}
