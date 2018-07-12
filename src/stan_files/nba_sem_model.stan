functions {
  vector get_types(int K,
                   int N_offense,
                   int[] idx,
                   vector[] dens) {
    vector[K] types = rep_vector(0, K);
    vector[K] prob;
    real max_prob;
    for (i in 1:N_offense) {
      prob = softmax(dens[idx[i]]);
      for (j in 1:K) {
        if (prob[j] == max(prob)) {
          types[j] += 1;
        }
      }
    }
    return types;
  }

  row_vector get_interactions(int K,
                              int N_offense,
                              int[] idx,
                              vector[] dens) {
    int N_interactions = choose(K + 1, 2);
    row_vector[N_interactions] interactions = rep_row_vector(0, N_interactions);
    vector[K] types = get_types(K, N_offense, idx, dens);
    int shift = 0;

    for (i in 1:K) {
      // define index shift for filling in interactions vector
      if (i > 1) {
        for (k in i:K) {
          shift += 1;
        }
      }

      for (j in i:K) {
        // set interactions = 1 if interaction is observed
        if (i == j && types[i] > 1) {
          interactions[j + shift] = 1;
        } else if (i != j && types[i] > 0 && types[j] > 0) {
          interactions[j + shift] = 1;
        }
      }
    }

    return interactions;
  }
}


data {
  // mixture model piece
  int D;          // number of dimensions for mixture model
  int K;          // number of mixture components
  int N_players;  // number of players
  vector[D] means; // means for prior on mu
  vector[D] player_covariates[N_players]; // player data
  real mu_var_hp;
  real sigma_hp;

  // regression piece
  int N_stints;   // number of stints
  int N_on_court[N_stints]; // number of players on court
  int N_offense[N_stints];
  int w[N_stints]; // weights for each row in the regression
  int is_home[N_stints];
  int player_idx[N_stints, max(N_on_court)]; // idx for on court players for each stint
  real y[N_stints]; // response
}

transformed data {
  int N_interactions = choose(K + 1, 2);
}

parameters {
  // mixture model piece
  simplex[K] theta;             // mixing proportions
  vector[D] mu[K];             // mixture component means
  cholesky_factor_corr[D] L[K]; // cholesky factor of correlation
  real<lower = 0> sigma_mm;

  // regression piece
  real intercept;
  real home_effect;
  vector[2 * N_players] player_betas; // have to add one for replacement player
  vector[N_interactions] type_betas;
  real<lower = 0> sigma;
}


transformed parameters {
  vector[K] dens[N_players];

  for (n in 1:N_players) {
    for (k in 1:K) {
      dens[n][k] = multi_normal_cholesky_lpdf(player_covariates[n] | mu[k], sigma_mm * L[k]);
    }
  }
}


model {
  real ps[K];
  real m;
  row_vector[N_interactions] interactions;

  // mixture model priors
  sigma_mm ~ normal(0, 10);
  for(k in 1:K){
    for (d in 1:D) {
      mu[k][d] ~ normal(means[d], mu_var_hp);
    }
    L[k] ~ lkj_corr_cholesky(sigma_hp);
  }

  // mixture model likelihood
  for (n in 1:N_players) {
    for (k in 1:K) {
      // increment log probability of the gaussian
      ps[k] = (
        log(theta[k]) +
        multi_normal_cholesky_lpdf(player_covariates[n] | mu[k], sigma_mm * L[k])
      );
    }
    target += log_sum_exp(ps);
  }

  // regression piece
  player_betas ~ normal(0, 0.01);
  type_betas ~ normal(0, 0.1);
  sigma ~ normal(0, 20);

  for (n in 1:N_stints) {
    interactions = get_interactions(K,
                                    N_offense[n],
                                    player_idx[n, 1:N_offense[n]],
                                    dens);
    m = (
      intercept +
      home_effect * is_home[n] +
      sum(player_betas[player_idx[n, 1:N_on_court[n]]]) +
      interactions * type_betas
    );
    target += w[n] * normal_lpdf(y[n] | m, sigma);
  }
}


generated quantities {
  int component_assignment[N_players];
  for (n in 1:N_players) {
    component_assignment[n] = categorical_logit_rng(dens[n]);
  }
}
