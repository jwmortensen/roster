library(dplyr)
library(Matrix)
library(glmnet)
library(rstan)
library(kingsutils)

# define global variables -------------------------------------------------

year_string = 2018

get_player_levels = function(pbp_df, subset = "all") {
  if (subset == "offense") {
    regex = "offensive_player_*"
  } else if (subset == "defense") {
    regex = "defensive_player_*"
  } else if (subset == "all") {
    regex = "*_player_*"
  }
  player_cols = pbp_df %>%
    ungroup() %>%
    select(matches(regex))
  lvls = as.character(sapply(player_cols, as.character))
  lvls
}


# format data -------------------------------------------------------------

load("./data/apm_data.RData")

pbp_df = apm_data$pbp %>%
  group_by(game_code , possession_id) %>%
  summarise(points_in_possession = mean(points_in_possession, na.rm=TRUE),

            # offensive players that are on for that possession
            offensive_player_one=unique(offensive_player_1)[1],
            offensive_player_two=unique(offensive_player_2)[1],
            offensive_player_three=unique(offensive_player_3)[1],
            offensive_player_four=unique(offensive_player_4)[1],
            offensive_player_five=unique(offensive_player_5)[1],

            # defensive players that are on for that possession
            defensive_player_one=unique(defensive_player_1)[1],
            defensive_player_two=unique(defensive_player_2)[1],
            defensive_player_three=unique(defensive_player_3)[1],
            defensive_player_four=unique(defensive_player_4)[1],
            defensive_player_five=unique(defensive_player_5)[1],

            is_home = unique(possession_team_is_home)[1]
  ) %>%
  ungroup()
pbp_df = pbp_df[complete.cases(pbp_df), ]

offensive_players = get_player_levels(pbp_df, subset = "offense")
defensive_players = get_player_levels(pbp_df, subset = "defense")

# * replace players who don't have stats ----------------------------------
unique_players = c(offensive_players, defensive_players) %>%
  unique() %>%
  sort()
# stats_players = apm_data$player %>%
#   select(player_code) %>%
#   distinct() %>%
#   pull()
# replacement_players = setdiff(unique_players, stats_players)

all_players = player_lvls = unique_players
# all_players = setdiff(unique_players, replacement_players)
n_players = length(all_players)
rep_player_code = "replacement_player"
# player_lvls = c(all_players, rep_player_code)

for (i in 1:5) {
  off_varname <- sprintf("offensive_player_%s", sportradar::num_to_text(i))
  off_var <- pbp_df %>%
    select(off_varname) %>%
    pull()
  off_var[!(off_var %in% all_players)] <- rep_player_code
  pbp_df[, off_varname] <- factor(off_var, levels = player_lvls)


  def_varname <- sprintf("defensive_player_%s", sportradar::num_to_text(i))
  def_var <- pbp_df %>%
    select(def_varname) %>%
    pull()
  def_var[!(def_var %in% all_players)] <- rep_player_code
  pbp_df[, def_varname] <- factor(def_var, levels = player_lvls)
}


# glmnet fit --------------------------------------------------------------

# create design matrix. notice that we remove the replacement_players from
# the matrix, so they get rolled into the intercept.
X1 <- sparse.model.matrix(~-1 + offensive_player_one, pbp_df)
X2 <- sparse.model.matrix(~-1 + offensive_player_two, pbp_df)
X3 <- sparse.model.matrix(~-1 + offensive_player_three, pbp_df)
X4 <- sparse.model.matrix(~-1 + offensive_player_four, pbp_df)
X5 <- sparse.model.matrix(~-1 + offensive_player_five, pbp_df)
X_o <- X1 + X2 + X3 + X4 + X5
X_o <- X_o[, 1:n_players]
colnames(X_o) <- paste0(all_players, "_offense")


X1 <- sparse.model.matrix(~-1 + defensive_player_one, pbp_df)
X2 <- sparse.model.matrix(~-1 + defensive_player_two, pbp_df)
X3 <- sparse.model.matrix(~-1 + defensive_player_three, pbp_df)
X4 <- sparse.model.matrix(~-1 + defensive_player_four, pbp_df)
X5 <- sparse.model.matrix(~-1 + defensive_player_five, pbp_df)
X_d <- X1 + X2 + X3 + X4 + X5
X_d <- X_d[, 1:n_players]
colnames(X_d) = paste0(all_players, "_defense")

X <- cBind(pbp_df$is_home, X_o, X_d)
y <- pbp_df$points_in_possession

system.time({fit <- cv.glmnet(X, y, alpha = 0, standardize = F)})
beta <- coef(fit, s = 'lambda.min')[, 1]

beta_o <- 100 * beta[3:(n_players + 2)]
beta_d <- 100 * beta[(n_players + 3):(2*n_players + 2)]

# glmnet results ----------------------------------------------------------

raw_rapm <- data.frame(player_code = all_players,
                       opm_glmnet = beta_o,
                       dpm_glmnet = beta_d,
                       rpm_glmnet = beta_o - beta_d)
rasch_rapm_df <- raw_rapm %>%
  left_join((apm_data$player %>%
               select(first_name, last_name, player_code) %>%
               distinct()), by = "player_code") %>%
  select(first_name,
         last_name,
         player_code,
         opm_glmnet,
         dpm_glmnet,
         rpm_glmnet) %>%
  arrange(desc(rpm_glmnet)) %>%
  mutate(rank_glmnet = 1:n())
head(rasch_rapm_df)
tail(rasch_rapm_df)


# weighted glmnet ---------------------------------------------------------

pbp_df_weighted = pbp_df %>%
  group_by(points_in_possession,
           is_home,
           offensive_player_one,
           offensive_player_two,
           offensive_player_three,
           offensive_player_four,
           offensive_player_five,
           defensive_player_one,
           defensive_player_two,
           defensive_player_three,
           defensive_player_four,
           defensive_player_five) %>%
  summarise(weight = n(), points_per_possession = sum(points_in_possession) / n()) %>%
  select(points_per_possession, weight, is_home, matches("*_player_*"))

# I need to only keep players that are in the boxscore data, replacing others
# with 'replacement_player.' I will also need to figure out what to do with
# the interactions for these players (probably automatically assign them to
# no component)
# we want to turn replacement_players into NA's so we use all_players instead
# of player_lvls
offense_levels = paste0(all_players, "_offense")
defense_levels = paste0(all_players, "_defense")
all_levels = c(offense_levels, defense_levels)

pbp_offensive_players = get_player_levels(pbp_df_weighted, subset = "offense")
offensive_players = pbp_offensive_players
# offensive_players[offensive_players %in% replacement_players] = rep_player_code
offense_labels = paste0(offensive_players, "_offense")

pbp_defensive_players = get_player_levels(pbp_df_weighted, subset = "defense")
defensive_players = pbp_defensive_players
# defensive_players[defensive_players %in% replacement_players] = rep_player_code
defense_labels = paste0(defensive_players, "_defense")

all_labels = factor(c(offense_labels, defense_labels), levels = all_levels)

player_idx_vec = as.numeric(all_labels)
player_idx = matrix(player_idx_vec,
                    nrow = nrow(pbp_df_weighted), ncol = 10, byrow = F)
num_on_offense = rowSums(!is.na(player_idx[, 1:5]))
num_on_court = rowSums(!is.na(player_idx))
# player_idx[] <- t(apply(player_idx, 1, function(x) c(x[!is.na(x)],x[is.na(x)]) ))

# player_idx_coords = cbind(rep(1:nrow(pbp_df_weighted), each=10),
#                           as.vector(t(player_idx)))
# player_idx_coords = player_idx_coords[complete.cases(player_idx_coords), ]
#
# player_idx[is.na(player_idx)] = 0
#
# X_w = sparseMatrix(player_idx_coords[, 1], player_idx_coords[, 2])
# X_w = cbind(pbp_df_weighted$is_home, X_w)
# y_w = pbp_df_weighted$points_in_possession
# w = pbp_df_weighted$weight
#
# system.time({fit_weighted = cv.glmnet(X_w, y_w, alpha = 0, standardize = F, weights = w)})
# beta <- coef(fit, s = 'lambda.min')[, 1]
#
# beta_o <- 100 * beta[3:(n_players + 2)]
# beta_d <- 100 * beta[(n_players + 3):(2*n_players + 2)]

# glmnet results ----------------------------------------------------------

# raw_rapm <- data.frame(player_code = all_players,
#                        opm_glmnet_w = beta_o,
#                        dpm_glmnet_w = beta_d,
#                        rpm_glmnet_w = beta_o - beta_d)
# rasch_rapm_weighted_df <- raw_rapm %>%
#   left_join((apm_data$player %>%
#                select(first_name, last_name, player_code) %>%
#                distinct()), by = "player_code") %>%
#   select(first_name,
#          last_name,
#          player_code,
#          opm_glmnet_w,
#          dpm_glmnet_w,
#          rpm_glmnet_w) %>%
#   arrange(desc(rpm_glmnet_w)) %>%
#   mutate(rank_glmnet_w = 1:n()) %>%
#   left_join(rasch_rapm_df, by = c("player_code", "first_name", "last_name"))
# head(rasch_rapm_weighted_df)
# tail(rasch_rapm_weighted_df)



# stan model --------------------------------------------------------------

standat = list(
  N_players = length(all_players),
  N_stints = nrow(pbp_df_weighted),
  N_on_court = 10, #num_on_court,
  N_offense = num_on_offense,
  w = pbp_df_weighted$weight,
  is_home = as.integer(pbp_df_weighted$is_home),
  player_idx = player_idx,
  y = pbp_df_weighted$points_per_possession
)

save(standat,
     all_levels,
     offense_levels,
     rasch_rapm_df,
     file = "./data/nba_rapm_input_points_per_possession.RData")

load("./data/nba_rapm_input_points_per_possession.RData")
options(mc.cores = 1)
rstan_options(auto_write = TRUE)
start_time = Sys.time()
stanout <- stan(file = "./src/stan_files/bayesian_apm_basic.stan",
                data = standat,
                chains = 1)
end_time = Sys.time()
print(end_time - start_time)

save(stanout, file = "./data/nba_rapm_raw_output_points_per_possession.RData")

params = extract(stanout)

# plot player betas
player_draws = params$player_betas
colnames(player_draws) = all_levels
player_betas = 100 * colMeans(player_draws)
opm = player_betas[1:length(offense_levels)]
dpm = player_betas[(length(offense_levels)+1):length(player_betas)]
rpm = opm - dpm
all_players = all_levels[grepl("*offense", all_levels)]
all_players = gsub("_offense", "", all_players)
rpm_df = data.frame(player_code = all_players, opm, dpm, rpm, row.names = NULL)
rpm_df = rpm_df %>%
  arrange(desc(rpm))

full_rpm = rpm_df %>%
  inner_join(rasch_rapm_df, by = c("player_code"))
runtime = end_time - start_time
save(stanout, full_rpm, runtime, file = "./data/apm_model_results_points_per_possession.RData")

head(full_rpm)
tail(full_rpm)


