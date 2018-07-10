data {
  // regression piece
  int N_players;  // number of players
  int N_stints;   // number of stints
  matrix[N_stints, N_players] X; // idx for on court players for each stint
  real y[N_stints]; // response
}

parameters {
  // regression piece
  real intercept;
  vector[N_players] beta;
  real<lower = 0> sigma;
}

model {
  real m;

  // regression piece
  beta ~ normal(0, 5);
  intercept ~ normal(0, 1);
  sigma ~ normal(0, 20);

  y ~ normal(intercept + X * beta, sigma);
}
