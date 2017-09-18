#' roster
#'
#' @name roster
#' @docType package
NULL


#' Play-by-play data
#'
#' Play by play data from a game between the Memphis Grizzlies and Sacramento
#' Kings on 1 January 2017.
#'
#'
#' @format A data frame with 525 rows and 75 variables
#' @name pbp
#' @docType data
NULL

#' Player data
#'
#' Player information for game participants
#'
#' \itemize{
#'   \item player_id
#'   \item full_name
#'   \item team_id
#'   \item team_market
#'   \item team_name
#'   \item season
#' }
#' @format a data frame with 23 rows and 6 variables
#' @name players
#' @docType data
NULL
#'

#' make sure play by play is sorted first by period, then by clock
#'
#' @param pbp_df a data.frame
#' @return data.frame
sort_pbp <- function(pbp_df) {
  `%>%` <- dplyr::`%>%`

  check_time_format <- function(x) {
    if (grepl(":(.*):", x)) {
      T
    } else if (grepl("^[^:]+:[^:]+$", x)) { # regex for only one colon
      F
    } else {
      stop("invalid time format")
    }
  }

  if (class(pbp_df$clock) != "Period") {
    num_hms <- sum(sapply(pbp_df$clock, check_time_format))
    if (num_hms == nrow(pbp_df)) {
      pbp_df$clock <- lubridate::hms(pbp_df$clock)
    } else if (num_hms == 0) {
      pbp_df$clock <- lubridate::ms(pbp_df$clock)
    } else {
      stop("inconsistent game clock format")
    }
  }
  # didn't use dplyr to sort because of weird conflicts with lubridate.
  # after sorting it was converting 12 m 0 s to 12 m 39 s.  ¯\_(ツ)_/¯
  pbp_df <- pbp_df[order(pbp_df$game_id, -pbp_df$period_sequence, pbp_df$clock, decreasing = T), ]
  pbp_df
}

#' checks to make sure data.frame is sorted properly
#'
#' @param pbp_df a data.frame
#' @return data.frame
check_sort <- function(pbp_df) {
  sorted <- order(pbp_df$game_id, -pbp_df$period_sequence, pbp_df$clock, decreasing = T)
  if (!isTRUE(all.equal(sorted, 1:nrow(pbp_df)))) {
    pbp_df <- sort_pbp(pbp_df)
  }
  pbp_df
}

#' assign id to each possession, which can be used to calculate posessions
#' for each lineup.
#'
#' @param pbp_df the play by play data.frame
#' @return data.frame that mirrors pbp_df but with an additional possession_id column
#' @export
assign_possession_ids <- function(pbp_df) {
  `%>%` <- dplyr::`%>%`
  pbp_df <- check_sort(pbp_df)
  lag_poss_team_id <- dplyr::lag(pbp_df$possession_team_id, default = "")
  lead_event_type <- dplyr::lead(pbp_df$event_type, default = "")
  lead2_event_type <- dplyr::lead(pbp_df$event_type, n = 2, default = "")
  lead_event_team_id <- dplyr::lead(pbp_df$event_team_id, default = "")
  end_possession <- ((pbp_df$shot_made == T &                               # this condition is here to grab made shots
                        pbp_df$event_type != "freethrowmade") |# &              # but not end the possession if there was a foul on the shot.
                        # (lead_event_type != "shootingfoul" |
                        #    (lead_event_type == "shootingfoul" & lead2_event_type
                        #    pbp_df$event_team_id == lead_event_team_id)) |   # this checks if the shooting foul was called on the same team that made the shot (i.e., on the next possession)
                     (pbp_df$event_type == "rebound" & pbp_df$rebound_type == "defensive") |
                     (pbp_df$event_type == "freethrowmade" &
                        grepl("(1 of 1|2 of 2|3 of 3)", pbp_df$event_description)) |
                     (pbp_df$event_type == "turnover") |
                     # (pbp_df$possession_team_id != lag_poss_team_id &
                     #    !is.na(pbp_df$possession_team_id) &
                     #    !is.na(lag_poss_team_id)) |
                     (pbp_df$event_type == "endperiod"))
  end_possession[is.na(end_possession)] <- F
  end_possession_events <- pbp_df %>%
    dplyr::filter(end_possession) %>%
    dplyr::select(event_id) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  start_possession_events <- pbp_df %>%
    dplyr::select(event_id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(lag_event = dplyr::lag(event_id)) %>%
    dplyr::filter(lag_event %in% end_possession_events) %>%
    dplyr::select(event_id) %>%
    dplyr::pull()
  pbp_df <- pbp_df %>%
    dplyr::group_by(game_id) %>%
    dplyr::mutate(lag_event = dplyr::lag(event_id, default = "")) %>%
    dplyr::mutate(possession_id = 1 + cumsum(event_id %in% start_possession_events &
                                             event_id != lag_event
                                             )) %>%
    dplyr::mutate(lead_poss = dplyr::lead(possession_id, default = "")) %>%
    dplyr::mutate(possession_team = if_else(lead_poss == possession_id, as.character(NA),
                                            if_else(possession_team_id == home_team_id,
                                            "AWAY", "HOME"))) %>%
    dplyr::mutate(possession_team = if_else(lead_poss == possession_id, as.character(NA),
                                            if_else(is.na(possession_team) & event_team_id == home_team_id,
                                                    "HOME", if_else(is.na(possession_team) & event_team_id != home_team_id,
                                                                    "AWAY", possession_team)))) %>%
    dplyr::group_by(game_id, possession_id) %>%
    dplyr::mutate(points_in_possession = sum(points, na.rm = T)) %>%
    dplyr::select(-lag_event, -lead_poss) %>%
    dplyr::ungroup()
  pbp_df
}


#' assign id to each lineup in order to calculate +/-
#'
#' @param pbp_df the play by play data.frame
#' @return data.frame that mirrors pbp_df but with an additional lineup_id column
#' @export
assign_lineup_ids <- function(pbp_df) {
  `%>%` <- dplyr::`%>%`
  pbp_df <- check_sort(pbp_df)
  # sort columns to avoid duplicate lineups
  # home_players <- pbp_df %>%
  #   dplyr::select(dplyr::matches("home_player(.*)id"))
  # home_players <- t(apply(home_players, 1, sort, na.last = T))
  # away_players <- pbp_df %>%
  #   dplyr::select(dplyr::matches("away_player(.*)id"))
  # away_players <- t(apply(away_players, 1, sort, na.last = T))
  # pbp_df <- pbp_df %>%
  #   dplyr::mutate(home_player_one_id = home_players[, 1],
  #                 home_player_two_id = home_players[, 2],
  #                 home_player_three_id = home_players[, 3],
  #                 home_player_four_id = home_players[, 4],
  #                 home_player_five_id = home_players[, 5],
  #                 away_player_one_id = away_players[, 1],
  #                 away_player_two_id = away_players[, 2],
  #                 away_player_three_id = away_players[, 3],
  #                 away_player_four_id = away_players[, 4],
  #                 away_player_five_id = away_players[, 5])
  lineup_df <- pbp_df %>%
    dplyr::select(dplyr::matches("(home|away)_player(.*)id")) %>%
    na.omit() %>%
    unique() %>%
    dplyr::mutate(lineup_id = 1:n())
  text_nums <- c("one", "two", "three", "four", "five")
  pbp_df <- dplyr::left_join(pbp_df, lineup_df,
                             by = c(paste0("home_player_", text_nums, "_id"),
                                    paste0("away_player_", text_nums, "_id")))
  # lag_event_type <- dplyr::lag(pbp_df$event_type, default = "")
  # lag2_event_type <- dplyr::lag(pbp_df$event_type, n = 2, default = "")
  # lag_clock <- dplyr::lag(pbp_df$clock)
  # lag2_clock <- dplyr::lag(pbp_df$clock, n = 2, default = "")
  # lag_lineup <- dplyr::lag(pbp_df$lineup_id, n = 2, default = 0)
  # lag2_lineup <- dplyr::lag(pbp_df$lineup_id, n = 3, default = 0)
  # pbp_df <- pbp_df %>%
  #   dplyr::mutate(lineup_id = ifelse(event_type == "freethrowmade" &
  #                                       lag_event_type == "lineupchange" &
  #                                       lag2_event_type != "lineupchange", lag_lineup,
  #                                     ifelse(event_type == "freethrowmade" &
  #                                               lag_event_type == "lineupchange" &
  #                                               lag2_event_type == "lineupchange", lag2_lineup,
  #                                             lineup_id)))
  pbp_df
}

#' Calculate PM for each lineup in a single game
#'
#' @param pbp_df the play by play data.frame
#' @return data.frame
#' @export
plus_minus <- function(pbp_df) {
  require(dplyr)
  pbp_df <- assign_possession_ids(pbp_df)
  pbp_df <- assign_lineup_ids(pbp_df)
  pls_min_df <- pbp_df %>%
    filter(!is.na(lineup_id), !is.na(possession_team)) %>%
    group_by(lineup_id) %>%
    summarise(home_possessions = sum(possession_team == "HOME"),
              home_points = sum(if_else(possession_team == "HOME", points_in_possession, as.integer(0))),
              home_points_per_poss = home_points / home_possessions,
              away_possessions = sum(possession_team == "AWAY"),
              away_points = sum(if_else(possession_team == "AWAY", points_in_possession, as.integer(0))),
              away_points_per_poss = away_points / away_possessions,
              total_possessions = home_possessions + away_possessions)
  avg_rating <- sum(pls_min_df$home_points, pls_min_df$away_points) /
    sum(pls_min_df$away_possessions, pls_min_df$home_possessions)
  pls_min_df <- pls_min_df %>%
    mutate(home_points_per_poss = if_else(home_possessions == 0,
                                          avg_rating, home_points_per_poss),
           away_points_per_poss = if_else(away_possessions == 0,
                                          avg_rating, away_points_per_poss)) %>%
    mutate(pls_min = 100 * (home_points_per_poss - away_points_per_poss))
  player_levels <- pbp_df %>%
    select(matches("(home|away)_player(.*)id")) %>%
    unlist() %>%
    na.omit() %>%
    unique()
  make_player_factor <- function(vec) {
    factor(vec, levels = player_levels, exclude = NULL)
  }
  player_df <- pbp_df %>%
    select(lineup_id, matches("(home|away)_player(.*)id")) %>%
    distinct() %>%
    mutate_at(vars(matches("player")), make_player_factor)
  pls_min_df <- left_join(pls_min_df, player_df, by = c("lineup_id"))
  pls_min_df
}

offdef_apm <- function(pbp_df, aggregate = F) {
  require(Matrix)
  require(dplyr)
  require(glmnet)

  process_row <- function(row) {
    home_player_ids <- row %>% select(matches("home_player(.*)id"))
    away_player_ids <- row %>% select(matches("away_player(.*)id"))
    if (row$possession_team == "HOME") {
      df <- data.frame(row$points_in_possession, home_player_ids, away_player_ids, stringsAsFactors = F)
    } else if (row$possession_team == "AWAY") {
      df <- data.frame(row$points_in_possession, away_player_ids, home_player_ids, stringsAsFactors = F)
    }
    text_nums <- c("one", "two", "three", "four", "five")
    names(df) <- c("points", paste0("offense_player_", text_nums, "_id"),
                   paste0("defense_player_", text_nums, "_id"))
    df
  }

  pbp_df <- assign_possession_ids(pbp_df)
  pbp_df <- assign_lineup_ids(pbp_df)
  pm_df <- pbp_df %>%
    filter(!is.na(possession_team))
  out_df <- as.data.frame(t(sapply(1:nrow(pm_df), function(i) { process_row(pm_df[i, ]) })))
  player_levels <- out_df %>%
    select(matches("player")) %>%
    unlist() %>%
    na.omit() %>%
    unique()
  make_player_factor <- function(vec) {
    factor(vec, levels = player_levels, exclude = NULL)
  }
  out_df <- out_df %>%
    mutate_at(vars(matches("player")), make_player_factor) %>%
    mutate(points = as.integer(points))
  if(aggregate) {
    out_df <- out_df %>%
      group_by_at(vars(matches("player"))) %>%
      summarise(points = sum(points), possessions = n()) %>%
      mutate(points_per_possession = points / possessions)
  }
  O1 <- sparse.model.matrix(~-1 + offense_player_one_id, out_df)
  O2 <- sparse.model.matrix(~-1 + offense_player_two_id, out_df)
  O3 <- sparse.model.matrix(~-1 + offense_player_three_id, out_df)
  O4 <- sparse.model.matrix(~-1 + offense_player_four_id, out_df)
  O5 <- sparse.model.matrix(~-1 + offense_player_five_id, out_df)
  O <- O1 + O2 + O3 + O4 + O5
  D1 <- sparse.model.matrix(~-1 + defense_player_one_id, out_df)
  D2 <- sparse.model.matrix(~-1 + defense_player_two_id, out_df)
  D3 <- sparse.model.matrix(~-1 + defense_player_three_id, out_df)
  D4 <- sparse.model.matrix(~-1 + defense_player_four_id, out_df)
  D5 <- sparse.model.matrix(~-1 + defense_player_five_id, out_df)
  D <- D1 + D2 + D3 + D4 + D5
  colnames(O) <- paste0("offense_", player_levels)
  colnames(D) <- paste0("defense_", player_levels)
  X <- cbind(O, D)
  y <- if (aggregate) 100 * out_df$points_per_possession else out_df$points
  w <- if (aggregate) out_df$possessions else rep(1, nrow(X))
  fit <- cv.glmnet(X, y, alpha = 0, weights = w, intercept = F)
  fit
}

#' calculate adjusted +/-
#'
#' @param pls_min_df a data.frame
#' @return data.frame
#' @export
apm <- function(pls_min_df, players_df, minutes_threshold = 0, weights = T) {
  require(glmnet)
  levels_list <- lapply(pls_min_df %>% select(matches("player(.*)id")), levels)
  if (length(unique(levels_list)) == 1) {
    X1 <- sparse.model.matrix(~-1 + home_player_one_id, pls_min_df)
    X2 <- sparse.model.matrix(~-1 + home_player_two_id, pls_min_df)
    X3 <- sparse.model.matrix(~-1 + home_player_three_id, pls_min_df)
    X4 <- sparse.model.matrix(~-1 + home_player_four_id, pls_min_df)
    X5 <- sparse.model.matrix(~-1 + home_player_five_id, pls_min_df)
    X6 <- -1 * sparse.model.matrix(~-1 + away_player_one_id, pls_min_df)
    X7 <- -1 * sparse.model.matrix(~-1 + away_player_two_id, pls_min_df)
    X8 <- -1 * sparse.model.matrix(~-1 + away_player_three_id, pls_min_df)
    X9 <- -1 * sparse.model.matrix(~-1 + away_player_four_id, pls_min_df)
    X10 <- -1 * sparse.model.matrix(~-1 + away_player_five_id, pls_min_df)
    X <- X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10
    colnames(X) <- levels_list[[1]]
    included_players <- players_df %>%
      filter(minutes > minutes_threshold) %>%
      select(player_id) %>%
      pull()
    X <- X[, colnames(X) %in% included_players]
    w <- if (weights) (pls_min_df$total_possessions) else rep(1, nrow(pls_min_df))
    fit <- glmnet(X, pls_min_df$pls_min,
                  weights = w,
                  alpha = 0,
                  intercept = F)
    fit
  } else {
    stop("at least one player_id column has mismatched factor levels.")
  }
}


