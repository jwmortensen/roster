data {
  int D;          // number of dimensions
  int K;          // number of gaussians
  int N;          // number of data
  vector[D] y[N]; // data
  vector[D] means; // means for prior on mu

  real mu_var_hp;
  real sigma_hp;
}

parameters {
  simplex[K] theta;             // mixing proportions
  vector[D] mu[K];              // mixture component means
  cholesky_factor_corr[D] L[K]; // cholesky factor of correlation
  real<lower = 0> sigma;
}

transformed parameters {
  vector[K] dens[N];

  for (n in 1:N) {
    for (k in 1:K) {
      dens[n][k] = multi_normal_cholesky_lpdf(y[n] | mu[k], sigma * L[k]);
    }
  }
}

model {
  real ps[K];
  for(k in 1:K){
    for (d in 1:D) {
      mu[k][d] ~ normal(means[d], mu_var_hp);
    }
    L[k] ~ lkj_corr_cholesky(sigma_hp); // sigma_hp > 1: more weight on identity,
                                        // sigma_hp == 1: uniform prior
                                        // sigma_hp < 1: more weight on off diagonals
  }
  sigma ~ normal(0, 5);

  for (n in 1:N){
    for (k in 1:K){
      // increment log probability of the gaussian
      ps[k] = log(theta[k]) + multi_normal_cholesky_lpdf(y[n] | mu[k], sigma * L[k]);
    }
    target += log_sum_exp(ps);

  }
}

generated quantities {
  int y_pred[N];
  for (n in 1:N) {
    y_pred[n] = categorical_logit_rng(dens[n]);
  }
}


