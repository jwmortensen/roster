data {
  // regression piece
  int N_players;  // number of players
  int N_on_court; // number of players on court
  int N_stints;   // number of stints
  int player_idx[N_stints, N_on_court]; // idx for on court players for each stint
  int w[N_stints]; // weights for each row in the regression
  int is_home[N_stints];
  real y[N_stints]; // response
}

parameters {
  // regression piece
  real intercept;
  real home_effect;
  vector[N_players] player_betas;
  real<lower = 0> sigma;
}

model {
  real m[N_stints];

  // regression piece
  player_betas ~ normal(0, 0.1);
  intercept ~ normal(0, 1);
  home_effect ~ normal(0, 1);
  sigma ~ normal(0, 20);

  for (n in 1:N_stints) {
    m[n] = (
      intercept +
      home_effect * is_home[n] +
      sum(player_betas[player_idx[n, ]])
    );
    target += w[n] * normal_lpdf(y[n] | m[n], sigma);
  }
}