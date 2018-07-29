library(dplyr)
library(lavaan)
library(glmnet)
library(sportradar)
library(ggplot2)


# global vars -------------------------------------------------------------

s_glmnet = 0.01
minutes_threshold = 500

# global functions -------------------------------------------------

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

create_rapm_df = function(fit) {
  beta = get_beta(fit)
  beta_o = beta[3:(n_players + 2)]
  beta_d = beta[(n_players + 3):(2*n_players + 2)]
  raw_rapm = data.frame(player_code = high_minute_players,
                        opm = beta_o,
                        dpm = beta_d,
                        rpm = beta_o - beta_d)
  rapm_df = raw_rapm %>%
    left_join((apm_data$player %>%
                 select(first_name, last_name, player_code) %>%
                 distinct()), by = "player_code") %>%
    select(first_name,
           last_name,
           player_code,
           opm,
           dpm,
           rpm) %>%
    arrange(desc(rpm))
  rapm_df
}

get_beta = function(fit) {
  # beta = coef(fit, s = s_glmnet)[, 1]
  beta = coef(fit, s = 'lambda.min')[, 1]
  beta
}

get_beta_o = function(fit) {
  beta = get_beta(fit)
  beta_o = 100 * beta[3:(n_players + 2)]
  beta_o
}

plot_loadings = function(mat, title = NULL) {
  melt_df = reshape2::melt(mat)
  colnames(melt_df) = c("factor", "variable", "loading")
  melt_df$factor = as.factor(melt_df$factor)
  gg = ggplot(melt_df, aes(x = variable,
                           y = loading,
                           col = factor)) +
    geom_point() +
    facet_wrap(~factor) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(title)
  print(gg)
}

# format data -------------------------------------------------------------

load("./data/apm_data.RData")


# * player stats ----------------------------------------------------------

player_df = apm_data$player
player_df = player_df %>%
  group_by(player_code) %>%
  filter(minutes == max(minutes)) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  filter(minutes > minutes_threshold)
mixture_df = player_df %>%
  mutate(fg_pct = (two_points_made + three_points_made) /
           (two_points_attempted + three_points_attempted),
         two_point_pct = two_points_made / two_points_attempted,
         two_points_per_36 = 36 * two_points_made / minutes,
         two_points_attempted_per_36 = 36 * two_points_attempted / minutes,
         three_point_pct = three_points_made / three_points_attempted,
         three_points_per_36 = 36 * three_points_made / minutes,
         three_points_attempted_per_36 = 36 * three_points_attempted / minutes,
         assists_per_36 = 36 * assists / minutes,
         steals_per_36 = 36 * steals / minutes,
         oreb_per_36 = 36 * offensive_rebounds / minutes,
         dreb_per_36 = 36 * defensive_rebounds / minutes,
         points_per_36 = 36 * points / minutes,
         turnovers_per_36 = 36 * turnovers / minutes,
         blocked_attempts_per_36 = 36 * blocked_shot_attempts / minutes,
         blocks_per_36 = 36 * blocked_shots / minutes,
         three_pt_attempts_per_36 = 36 * three_points_attempted / minutes,
         free_throw_pct = free_throws_made / free_throws_attempted,
         total_rebounds_per_36 = 36 * (offensive_rebounds + defensive_rebounds) / minutes,
         fouls_per_36 = 36 * personal_fouls / minutes,
         ato_ratio = assists / turnovers) %>%
  mutate(fg_pct = ifelse(is.na(fg_pct), quantile(fg_pct, 0.1, na.rm = T), fg_pct),
         fg_pct = ifelse(fg_pct == 1 | fg_pct == 0, mean(fg_pct), fg_pct),
         three_point_pct = ifelse(is.na(three_point_pct),
                                  quantile(three_point_pct, 0.1, na.rm = T),
                                  three_point_pct),
         three_point_pct = ifelse(three_point_pct == 1 | three_point_pct == 0,
                                  mean(three_point_pct),
                                  three_point_pct)) # %>%
  # mutate(steals_per_36 = ifelse(steals_per_36 > quantile(steals_per_36, 0.95, na.rm = T),
  #                               quantile(steals_per_36, 0.95, na.rm = T),
  #                               steals_per_36))

offense_df = mixture_df %>%
  select(fg_pct,
         three_point_pct,
         three_points_attempted_per_36,
         # two_point_pct,
         # two_points_attempted_per_36,
         free_throw_pct,
         free_throw_rate,
         assists_per_36,
         turnovers_per_36,
         # ato_ratio,
         # steals_per_36,
         oreb_per_36,
         # dreb_per_36,
         points_per_36,
         # blocked_attempts_per_36,
         fouls_per_36)
         # blocks_per_36)
defense_df = mixture_df %>%
  select(steals_per_36,
         dreb_per_36,
         fouls_per_36,
         blocks_per_36)


offense_scaled = as.data.frame(scale(offense_df))
defense_scaled = as.data.frame(scale(defense_df))


# * play by play ----------------------------------------------------------


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

# limit it to players above the minutes_threshold

all_players = c(offensive_players, defensive_players) %>%
  unique() %>%
  sort()

high_minute_players = sort(unique(player_df$player_code))
replacement_players = setdiff(all_players, high_minute_players)
replacement_player_code = "replacement_player"

player_lvls = c(high_minute_players, replacement_player_code)
n_players = length(high_minute_players)

for (i in 1:5) {
  off_varname = sprintf("offensive_player_%s", num_to_text(i))
  off_var = pbp_df %>%
    select(off_varname) %>%
    pull()
  off_var[off_var %in% replacement_players] = replacement_player_code
  pbp_df[, off_varname] = factor(off_var, levels = player_lvls)


  def_varname = sprintf("defensive_player_%s", num_to_text(i))
  def_var = pbp_df %>%
    select(def_varname) %>%
    pull()
  def_var[def_var %in% replacement_players] = replacement_player_code
  pbp_df[, def_varname] = factor(def_var, levels = player_lvls)
}

# weighted per possession data
pbp_df_weighted = pbp_df %>%
  group_by(is_home,
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
  summarise(weight = n(), ppp = sum(points_in_possession) / n()) %>%
  select(points_per_possession = ppp, weight, is_home, matches("*_player_*"))

offense_levels = paste0(player_lvls, "_offense")
defense_levels = paste0(player_lvls, "_defense")
all_levels = c(offense_levels, defense_levels)

offensive_players = get_player_levels(pbp_df_weighted, subset = "offense")
offense_labels = paste0(offensive_players, "_offense")

defensive_players = get_player_levels(pbp_df_weighted, subset = "defense")
defense_labels = paste0(defensive_players, "_defense")

all_labels = factor(c(offense_labels, defense_labels), levels = all_levels)

player_idx_vec = as.numeric(all_labels)
player_idx = matrix(player_idx_vec,
                    nrow = nrow(pbp_df_weighted), ncol = 10, byrow = F)

player_idx_coords = cbind(rep(1:nrow(pbp_df_weighted), each=10),
                          as.vector(t(player_idx)))

player_idx[is.na(player_idx)] = 0

X_w = sparseMatrix(player_idx_coords[, 1], player_idx_coords[, 2])
X_w = cbind(pbp_df_weighted$is_home, X_w)
colnames(X_w) = c("is_home", all_levels)
X_w = X_w[, !(colnames(X_w) %in%
                c(paste0(replacement_player_code, c("_offense", "_defense"))))]
y_w = pbp_df_weighted$points_per_possession
w = pbp_df_weighted$weight



# decomposition methods ---------------------------------------------------

# * NMF -------------------------------------------------------------------

# estimate_rank = NMF::nmfEstimateRank(as.matrix(mixture_df), range = 4:10)
# nmf_fit = NMF::nmf(as.matrix(mixture_df), rank = 6)
# plot_loadings(nmf_fit@fit@H)
# nmf_factor_scores = data.frame(player_code = player_df$player_code,
#                                nmf_fit@fit@W)


# * EFA -------------------------------------------------------------------

efa_df = mixture_scaled %>% select(-ato_ratio)
N_factors = 8
corrplot::corrplot(cor(efa_df))
factout = factanal(efa_df,
                   factors = N_factors,
                   rotation = "varimax",
                   scores = "regression",
                   )
plot_loadings(t(factout$loadings)[1:N_factors, 1:ncol(efa_df)], "EFA")
efa_factor_scores = data.frame(player_code = player_df$player_code,
                               factout$scores)


# * CFA -------------------------------------------------------------------

# cfa_model = '
#   rebounding =~ total_rebounds_per_36 +
# oreb_per_36 +
# dreb_per_36 +
# offensive_rebounding_percentage +
# defensive_rebounding_percentage
# ball_handling =~
# assists_per_36 +
# turnovers_per_36 +
# assist_percentage +
# turnover_percentage
# sharpshooting =~
# free_throw_pct +
# three_point_pct +
# fg_pct
# three_point_shooting =~
# three_points_per_36 +
# three_point_pct +
# three_point_attempt_rate
# two_point_shooting =~
# two_points_per_36 +
# two_point_pct
# carelessness =~
# turnovers_per_36 +
# turnover_percentage +
# blocked_attempts_per_36 +
# fouls_per_36
# ballhawk =~
# steals_per_36 +
# steal_percentage
# shot_blocker =~
# blocks_per_36 +
# block_percentage
# point_creator =~
# points_per_36 +
# assists_per_36
# '

cfa_model = '
  big_guy_stuff =~
    fg_pct +
    dreb_per_36 +
    oreb_per_36 +
    blocks_per_36 +
    free_throw_rate
  sharpshooter =~
    free_throw_pct +
    three_point_pct +
    points_per_36
  guards =~
    ato_ratio +
    steals_per_36 +
    fouls_per_36 +
    blocks_per_36
'

cfa_fit = lavaan::cfa(cfa_model, as.data.frame(mixture_scaled))
summary(cfa_fit)
cfa_parameter_estimates = lavaan::parameterEstimates(cfa_fit)
cfa_factors = cfa_parameter_estimates %>%
  filter(op == "=~")
facs = sort(unique(cfa_factors$lhs))
vars = sort(unique(cfa_factors$rhs))
n_factors = length(facs)
n_vars = length(vars)
cfa_loadings_mat = matrix(0,
                          nrow = n_factors,
                          ncol = n_vars,
                          dimnames = list(facs, vars))
for (i in 1:nrow(cfa_factors)) {
  cfa_loadings_mat[cfa_factors$lhs[i], cfa_factors$rhs[i]] = cfa_factors$est[i]
}
plot_loadings(cfa_loadings_mat)
cfa_factor_scores = data.frame(player_code = player_df$player_code,
                               lavaan::lavPredict(fit))


# RAPM --------------------------------------------------------------------

sum_factor_scores = function(factor_scores) {
  X_o = X_w[, 2:(n_players + 1)]
  factor_sums = t(apply(X_o, 1, function(x) {
    colSums(factor_scores[as.logical(x), -1])
  }))
  as.data.frame(factor_sums)
}

sum_offensive_player_stats = function(player_stats) {
  X_o = X_w[, 2:(n_players + 1)]
  stat_sums = t(apply(X_o, 1, function(x) {
    colSums(player_stats[as.logical(x), ])
  }))
  as.data.frame(stat_sums)
}

sum_defensive_player_stats = function(player_stats) {
  X_d = X_w[, (n_players + 2):ncol(X_w)]
  stat_sums = t(apply(X_d, 1, function(x) {
    colSums(player_stats[as.logical(x), ])
  }))
  as.data.frame(stat_sums)
}

fit_with_factors = function(offense_sums, defense_sums) {
  # poly_mat = NULL
  # for (i in 1:ncol(factor_sums)) {
  #   poly_mat = cbind(poly_mat, poly(factor_sums[, i], 2, raw = TRUE))
  # }
  # colnames(poly_mat) = t(outer(colnames(factor_sums), c("", "^2"), FUN = paste0))
  # interaction_mat = model.matrix(~-1 + .^2., factor_sums)[, -c(1:ncol(factor_sums))]
  # model_mat = cbind(poly_mat, interaction_mat)
  off_mat = model.matrix(~-1 + .^2, offense_sums)
  def_mat = model.matrix(~-1 + .^2, defense_sums)
  factor_fit = cv.glmnet(cbind(X_w, off_mat,
                               def_mat),
                         y_w, alpha = 0,
                         standardize = F,
                         weights = w)
  factor_fit
}


# * base model ------------------------------------------------------------

base_fit = cv.glmnet(X_w, y_w, alpha = 0,
                     standardize = F,
                     weights = w)
base_beta = get_beta(base_fit)
rapm_df = create_rapm_df(base_fit)
head(rapm_df)
tail(rapm_df)


# * NMF model -------------------------------------------------------------

# nmf_sum = sum_factor_scores(nmf_factor_scores)
# nmf_fit = fit_with_factors(nmf_sum)
#
# rapm_nmf = create_rapm_df(nmf_fit)
# head(rapm_nmf)
# tail(rapm_nmf)


# * EFA model -------------------------------------------------------------

# efa_sum = sum_factor_scores(efa_factor_scores)
# efa_fit = fit_with_factors(efa_sum)
# efa_beta = 100 * get_beta(efa_fit)
# factor_coef = tail(efa_beta, n = 44)
# squared_terms = factor_coef[seq(2, 16, by = 2)]
# linear_terms = factor_coef[seq(1, 15, by = 2)]
# coeff_mat = diag(linear_terms)
# coeff_mat[lower.tri(coeff_mat)] = factor_coef[-(1:(2*N_factors))]
# dimnames(coeff_mat) = list(paste0("factor", 1:N_factors),
#                            paste0("factor", 1:N_factors))
# corrplot::corrplot(coeff_mat, is.corr = FALSE)
# efa_rapm = create_rapm_df(efa_fit)
# head(efa_rapm)
# tail(efa_rapm)

# * CFA model -------------------------------------------------------------

# cfa_sum = sum_factor_scores(cfa_factor_scores)
# cfa_fit = fit_with_factors(cfa_sum)
# cfa_beta = 100 * get_beta(cfa_fit)
# tail(cfa_beta)
# cfa_rapm = create_rapm_df(cfa_fit)
# head(cfa_rapm)
# tail(cfa_rapm)



# * raw stats -------------------------------------------------------------

offensive_sums = sum_offensive_player_stats(offense_scaled)
defensive_sums = sum_defensive_player_stats(defense_scaled)
colnames(defensive_sums)[3] = "def_fouls_per_36"
stat_sums = cbind(offensive_sums, defensive_sums)
stat_fit = fit_with_factors(offensive_sums, defensive_sums)
(stat_mse = min(stat_fit$cvm))
stat_beta = get_beta(stat_fit)
k = ncol(stat_sums)
player_betas = stat_beta[3:(2*n_players + 2)]
non_player_betas = stat_beta[(2*n_players+3):length(stat_beta)]
stat_rapm = create_rapm_df(stat_fit)
head(stat_rapm)
tail(stat_rapm)

100 * non_player_betas[order(abs(non_player_betas), decreasing = TRUE)]


# * fit residuals on stats ------------------------------------------------

yhat = predict(base_fit, X_w, s = s_glmnet)
resid = y_w - yhat

# poly_mat = NULL
# for (i in 1:ncol(stat_sums)) {
#   poly_mat = cbind(poly_mat, poly(stat_sums[, i], 2, raw = TRUE))
# }
# colnames(poly_mat) = t(outer(colnames(stat_sums), c("", "^2"), FUN = paste0))
# interaction_mat = model.matrix(~-1 + .^2., stat_sums)[, -c(1:ncol(stat_sums))]
# model_mat = cbind(poly_mat, interaction_mat)
model_mat = model.matrix(~-1 + .^2, stat_sums)
resid_fit = cv.glmnet(model_mat, resid, alpha = 0,
                       standardize = F,
                       weights = w)
resid_beta = 100 * coef(resid_fit, s = 'lambda.min')[-1, 1]
resid_fit$cvm %>% min()


# * fit stats only --------------------------------------------------------

stats_only_fit = cv.glmnet(model_mat, y_w,
                           alpha = 0,
                           standardize = FALSE,
                           weights = w)
stats_only_mse = min(stats_only_fit$cvm)
stats_only_beta =  coef(stats_only_fit, s = s_glmnet)[-1, 1]

compare = cbind(stats_only = stats_only_beta,
                joint = non_player_betas,
                diff = stats_only_beta - non_player_betas)

stats_yhat = predict(stats_only_fit, model_mat, s = 'lambda.min')
stats_only_resid = y_w - stats_yhat



# * fit residuals on players ----------------------------------------------

player_resid_fit = cv.glmnet(X_w, stats_only_resid,
                             alpha = 0,
                             standardize = FALSE,
                             weights = w)
player_resid_rapm_df = create_rapm_df(player_resid_fit)
head(player_resid_rapm_df)
tail(player_resid_rapm_df)



rapm_df$rank = 1:nrow(rapm_df)
stat_rapm$rank = 1:nrow(stat_rapm)
all_rapm = rapm_df %>%
  left_join(stat_rapm,
            by = c("player_code", "first_name", "last_name"),
            suffix = c("", "_w_stats"))
all_rapm$player_code = factor(all_rapm$player_code, levels = rapm_df$player_code)

plot.default(all_rapm$player_code, all_rapm$rpm,
             axes = F,
             type = "n", pch = 1,
             xlab = "Player Code", ylab = "RPM",
             main = "Change in RPM",
             ylim = c(-5, 5))
arrows(x0 = as.numeric(all_rapm$player_code),
       y0 = all_rapm$rpm,
       y1 = all_rapm$rpm_w_stats,
       code = 2,
       length = 0.075,
       col = ifelse(all_rapm$rpm > all_rapm$rpm_w_stats, "red", "green"))
axis(side = 1, at = as.numeric(rapm_df$player_code), las = 2, labels = rapm_df$player_code)
axis(side = 2)

plot(rapm_df$player_code, all_rapm$rpm - all_rapm$rpm_w_stats, las = 2, axes = FALSE)
label_idx = round(seq(1, n_players, length = 10))
axis(side = 1,
     at = as.numeric(rapm_df$player_code)[label_idx],
     las = 2,
     labels = rapm_df$player_code[label_idx])
axis(side = 2)




plot(rapm_df$player_code, all_rapm$rank - all_rapm$rank_w_stats, axes = FALSE)
label_idx = round(seq(1, n_players, length = 10))
axis(side = 1,
     at = as.numeric(rapm_df$player_code)[label_idx],
     las = 2,
     labels = rapm_df$player_code[label_idx])
axis(side = 2)


# * create rapm -----------------------------------------------------------

rapm_df = create_rapm_df(base_fit)
stat_rapm = create_rapm_df(stat_fit)
rapm_df$rank = 1:nrow(rapm_df)
stat_rapm$rank = 1:nrow(stat_rapm)
all_rapm = rapm_df %>%
  left_join(stat_rapm,
            by = c("player_code", "first_name", "last_name"),
            suffix = c("", "_w_stats"))
all_rapm$player_code = factor(all_rapm$player_code, levels = rapm_df$player_code)


# * compare lebron on the lakers ------------------------------------------
lakers_lineup = c("rondora01", "beaslmi01", "jamesle01", "mcgeeja01", "stephla01")
lakers_factor = factor(lakers_lineup, levels = high_minute_players)
player_mat = sparseMatrix(i = rep(1, 5),
                          j = as.numeric(lakers_factor),
                          dims = c(1, n_players))

lakers_X_o = cbind(0, player_mat, rep(0, n_players))
(lebron_base_off = 100 * predict(base_fit, newx = lakers_X_o, s = 'lambda.min'))
lakers_X_d = cbind(0, rep(0, n_players), player_mat)
(lebron_base_def = 100 * predict(base_fit, newx = lakers_X_d, s = 'lambda.min'))

(lakers_df = all_rapm %>%
  filter(player_code %in% lakers_lineup))
lakers_player_opm = lakers_df %>%
  select(opm) %>% pull()
lakers_player_dpm = lakers_df %>%
  select(dpm) %>% pull()
lakers_player_rpm = lakers_df %>%
  select(rpm) %>% pull()
(lakers_lineup_opm = 100 * sum(c(lakers_player_opm)))
(lakers_lineup_dpm = 100 * sum(c(lakers_player_dpm)))
(lakers_lineup_rpm = 100 * sum(c(lakers_player_rpm)))


lakers_offense_stats = t(colSums(offense_scaled[as.logical(), ]))
lakers_defense_stats = t(colSums(defense_scaled[as.logical(lakers_X[, 2:(n_players+1)]), ]))
colnames(lakers_defense_stats)[3] = "def_fouls_per_36"


# poly_mat = NULL
# for (i in 1:ncol(stat_sums)) {
#   poly_mat = cbind(poly_mat, poly(stat_sums[, i], 2, raw = TRUE))
# }
# colnames(poly_mat) = t(outer(colnames(stat_sums), c("", "^2"), FUN = paste0))
# interaction_mat = t(model.matrix(~-1 + .^2., stat_sums)[, -c(1:ncol(stat_sums))])
# model_mat = cbind(poly_mat, interaction_mat)

offense_mat = model.matrix(~-1 + .^2, as.data.frame(lakers_offense_stats))
defense_mat = model.matrix(~-1 + .^2, as.data.frame(lakers_defense_stats))
(lebron_stats_off = predict(stat_fit,
                           newx = cbind(lakers_X_o,
                                        offense_mat,
                                        t(rep(0, ncol(defense_mat)))),
                           s='lambda.min'))

(lebron_stats_def = predict(stat_fit,
                           newx = cbind(lakers_X_d,
                                        t(rep(0, ncol(offense_mat))),
                                          defense_mat),
                           s='lambda.min'))
(lakers_players_stat_opm = lakers_df %>%
  pull(opm_w_stats))
(lakers_players_stat_dpm = lakers_df %>%
  pull(dpm_w_stats))
(lakers_players_stat_rpm = lakers_df %>%
  pull(rpm_w_stats))
(player_sum_off = sum(lakers_players_stat_opm))
(player_sum_def = sum(lakers_players_stat_dpm))
(stat_sum_off = c(offense_mat, rep(0, ncol(defense_mat))) %*% non_player_betas)
(stat_sum_def = c(rep(0, ncol(offense_mat)), defense_mat) %*% non_player_betas)

(stat_beta[1] + sum(lakers_players_stat_opm) + stat_sum_off)
(stat_beta[1] + sum(lakers_players_stat_dpm) + stat_sum_def)

off_contributions = c(offense_mat, rep(0, ncol(defense_mat))) * non_player_betas
off_contributions[order(abs(off_contributions), decreasing = TRUE)] * 100


(def_contributions = c(rep(0, ncol(offense_mat)), defense_mat) * non_player_betas)
(def_contributions = def_contributions[order(abs(def_contributions), decreasing = TRUE)][def_contributions != 0])
100 * def_contributions

100 * non_player_betas[order(abs(non_player_betas), decreasing = TRUE)]

lakers_stats_df = cbind(player_code = mixture_df$player_code, defense_scaled) %>%
  arrange(desc(steals_per_36)) %>%
  mutate(steals_rank = 1:n()) %>%
  arrange(desc(dreb_per_36)) %>%
  mutate(dreb_rank = 1:n()) %>%
  arrange(desc(blocks_per_36)) %>%
  mutate(blocks_rank = 1:n()) %>%
  filter(player_code %in% lakers_lineup) %T>%
  View()
