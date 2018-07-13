library(dplyr)
library(rstan)

load("./data/apm_data.RData")
player_df <- apm_data$player

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

plot_mixture_output <- function(stanout, mixture_df, K) {
  params <- extract(stanout)
  D = dim(params$mu)[3]
  cnames <- paste0("var", 1:D)
  mu1 <- as.data.frame(params$mu[, 1, ])
  names(mu1) <- cnames
  mu_df <- cbind(mu1, component = as.character(1), type = "mu")
  for (i in 2:K) {
    mu_i <- as.data.frame(params$mu[, i, ])
    names(mu_i) <- cnames
    mu_df <- rbind(
      mu_df,
      cbind(mu_i, component = as.character(i), type = "mu")
    )
  }
  names(mixture_df) <- cnames
  y_mode <- apply(params$y_pred, 2, Mode)
  mu_df <- rbind(
    mu_df,
    cbind(mixture_df,
          component = as.character(y_mode),
          type = "obs")
  )


  if (D < 3) {
    points <- ggplot(mu_df, aes(x = var1, y = var2, col = component, pch = type)) +
      geom_point() +
      labs(title = "Mixture component means")

    mu_df_no_obs <- mu_df %>% filter(type != "obs")
    contours <- ggplot(mu_df_no_obs, aes(x = var1, y = var2, col = component)) +
      stat_density_2d() +
      labs(title = "Mixture component means")
    gridExtra::grid.arrange(points, contours, nrow = 1, ncol = 2)
  } else {
    boxplots <- NULL

  }

}


mixture_df <- player_df %>%
  mutate(fg_pct = (two_points_made + three_points_made) /
                  (two_points_attempted + three_points_attempted),
         assists_per_36 = 36 * assists / minutes) %>%
  mutate(fg_pct = ifelse(is.na(fg_pct), quantile(fg_pct, 0.1, na.rm = T), fg_pct),
         fg_pct = ifelse(fg_pct == 1 | fg_pct == 0, mean(fg_pct), fg_pct)) %>%
  mutate(height_scale = as.numeric(scale(height)),
         weight_scale = as.numeric(scale(weight)),
         fg_pct_scale = as.numeric(scale(fg_pct)),
         assists_per_36_scale = as.numeric(scale(assists_per_36))) %>%
  select(height_scale, weight_scale, fg_pct_scale, assists_per_36_scale)
  # select(height, weight, fg_pct)

melt_mixture_df = reshape2::melt(mixture_df)
ggplot(melt_mixture_df, aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scale = "free")

N <- nrow(mixture_df)
K <- 3
D <- ncol(mixture_df)
standat <- list(N = N,
                D = D,
                K = K,
                y = mixture_df,
                means = colMeans(mixture_df),
                mu_var_hp = 50,
                sigma_hp = 4)
init_function <- function(chain_id) {
  list(theta = rep(1/K, K),
       mu = matrix(rnorm(K * D,
                         mean = apply(mixture_df, 2, function(x) {
                           quantile(x, probs = seq(0.1, 0.9, length = K)) } ),
                         sd = 0.001),
                   nrow = K, ncol = D, byrow = F))
}

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
stanout <- rstan::stan(file = "./src/stan_files/full_mixture_model.stan",
                data = standat,
                chains = 1,
                init = init_function)


plot_mixture_output(stanout, mixture_df,  K = K)
mu <- extract(stanout)$mu

create_mu_df = function(mu) {
  convert_mu = function(mu, i) {
    dat = cbind(as.data.frame(mu[ , i, ]), component = as.character(i))
    names(dat) = c(names(mixture_df), "component")
    dat
  }

  mu_df = convert_mu(mu, 1)
  for (i in 2:dim(mu)[2]) {
    mu_df = rbind(mu_df, convert_mu(mu, i))
  }
  mu_df = reshape2::melt(mu_df)
  mu_df
}

mu_df = create_mu_df(mu)

ggplot(mu_df, aes(x = variable,
                  y = value,
                  fill = component)) +
  geom_boxplot() +
  facet_wrap(~variable, scale = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
mean(extract(stanout)$lp__)
# -5022.227 for 3 components - 252.829 seconds to fit
# -4901.213 for 4 components - 412.872 seconds to fit
# -4809.884 for 5 components - 904.269 seconds to fit - component means less unimodal

library(rstan)
