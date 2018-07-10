functions {
  vector get_types(int K, int N_on_court, int[] idx, vector[] dens) {
    vector[K] types = rep_vector(0, K);
    vector[K] prob;
    real max_prob;
    for (i in 1:N_on_court) {
      prob = softmax(dens[idx[i]]);
      for (j in 1:K) {
        if (prob[j] == max(prob)) {
          types[j] += 1;
        }
      }
    }
    return types;
  }

  row_vector get_interactions(int K, int N_on_court, int[] idx, vector[] dens) {
    int N_interactions = choose(K+1, 2);
    row_vector[N_interactions] interactions = rep_row_vector(0, N_interactions);
    vector[K] types = get_types(K, N_on_court, idx, dens);
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

  // regression piece
  int N_on_court; // number of players on court
  int N_stints;   // number of stints
  int player_idx[N_stints, N_on_court]; // idx for on court players for each stint
  real y[N_stints]; // response
}

parameters {
  // mixture model piece
  simplex[K] theta;             // mixing proportions
  vector[D] mu[K];             // mixture component means
  cholesky_factor_corr[D] L[K]; // cholesky factor of correlation

  // regression piece
  vector[N_players] player_betas;
  real<lower = 0> sigma;
}


transformed parameters {
  vector[K] dens[N_players];

  for (n in 1:N_players) {
    for (k in 1:K) {
      dens[n][k] = multi_normal_cholesky_lpdf(player_covariates[n] | mu[k], L[k]);
    }
  }
}


model {
  real ps[K];
  real m;

  // mixture piece
  for(k in 1:K){
    for (d in 1:D) {
      mu[k][d] ~ normal(means[d], 50);
    }
    L[k] ~ lkj_corr_cholesky(4);
  }

  for (n in 1:N_players){
    for (k in 1:K){
      // increment log probability of the gaussian
      ps[k] = (
        log(theta[k]) +
        multi_normal_cholesky_lpdf(player_covariates[n] | mu[k], L[k])
      );
    }
    target += log_sum_exp(ps);
  }

  // regression piece
  player_betas ~ normal(0, 5);
  sigma ~ normal(0, 20);

  for (n in 1:N_stints) {
    m = (
      sum(player_betas[player_idx[n, ]])
    );
    y[n] ~ normal(m, sigma);
  }
}


generated quantities {
  int component_assignment[N_players];
  for (n in 1:N_players) {
    component_assignment[n] = categorical_logit_rng(dens[n]);
  }
}
