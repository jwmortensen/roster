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
#' @param pbp_df
#' @return data.frame
sort_pbp <- function(pbp_df) {
  `%>%` <- dplyr::`%>%`
  clock <- pbp_df$clock

  convert_time <- function(x) {
    if (grepl(":(.*):", x)) {
      lubridate::hms(x)
    } else if (grepl("^[^:]+:[^:]+$", x)) { # regex for only one colon
      lubridate::ms(x)
    } else {
      stop("not a valid time input.\n")
    }
  }

  if (class(clock) != "Period") {
    new_clock <- rep(lubridate::hms("00:12:00"), length(clock))
    # don't use sapply or mutate here because of weird lubridate conversion problems
    for (i in 1:length(clock)) {
      new_clock[i] <- convert_time(clock[i])
    }
    pbp_df$clock <- new_clock
  }
  pbp_df <- pbp_df[order(pbp_df$game_id, -pbp_df$period_sequence, pbp_df$clock, decreasing = T), ]
  pbp_df
}

#' checks to make sure data.frame is sorted properly
#'
#' @param pbp_df
#' @return data.frame
check_sort <- function(pbp_df) {
  sorted <- order(-pbp_df$period_sequence, pbp_df$clock, decreasing = T)
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
  end_possession <- ((pbp_df$shot_made == T & pbp_df$event_type != "freethrowmade") |
                     (pbp_df$event_type == "rebound" & pbp_df$rebound_type == "defensive") |
                     (pbp_df$event_type == "freethrowmade" & grepl("(1 of 1|2 of 2|3 of 3)", pbp_df$event_description)) |
                     (pbp_df$event_type == "turnover") |
                     (pbp_df$event_type == "endperiod"))
  end_possession[is.na(end_possession)] <- F
  end_possession_events <- pbp_df %>%
    dplyr::filter(end_possession) %>%
    dplyr::select(event_id) %>%
    dplyr::pull()
  pbp_df <- pbp_df %>%
    dplyr::mutate(lag_event = dplyr::lag(event_id, default = "")) %>%
    dplyr::mutate(possession_id = 1 + cumsum(event_id %in% end_possession_events &
                                             event_id != lag_event &
                                             !is.na(possession_team_id))) %>%
    dplyr::mutate(lag_poss = dplyr::lag(possession_id, default = "")) %>%
    dplyr::mutate(possession_team = if_else(lag_poss == possession_id, as.character(NA),
                                            if_else(possession_team_id == home_team_id,
                                            "HOME", "AWAY"))) %>%
    dplyr::select(-lag_event, -lag_poss)
  pbp_df
}


assign_lineup_ids <- function(pbp_df) {
  `%>%` <- dplyr::`%>%`
  pbp_df <- check_sort(pbp_df)
  # sort columns to avoid duplicate lineups
  home_players <- pbp_df %>%
    dplyr::select(dplyr::matches("home_player(.*)id"))
  home_players <- t(apply(home_players, 1, sort, na.last = T))
  away_players <- pbp_df %>%
    dplyr::select(dplyr::matches("away_player(.*)id"))
  away_players <- t(apply(away_players, 1, sort, na.last = T))
  pbp_df <- pbp_df %>%
    dplyr::mutate(home_player_one_id = home_players[, 1],
                  home_player_two_id = home_players[, 2],
                  home_player_three_id = home_players[, 3],
                  home_player_four_id = home_players[, 4],
                  home_player_five_id = home_players[, 5],
                  away_player_one_id = away_players[, 1],
                  away_player_two_id = away_players[, 2],
                  away_player_three_id = away_players[, 3],
                  away_player_four_id = away_players[, 4],
                  away_player_five_id = away_players[, 5])
  lineup_df <- pbp_df %>%
    dplyr::select(dplyr::matches("(home|away)_player(.*)id")) %>%
    na.omit() %>%
    unique() %>%
    dplyr::mutate(lineup_id = 1:n())
  text_nums <- c("one", "two", "three", "four", "five")
  pbp_df <- left_join(pbp_df, lineup_df,
                      by = c(paste0("home_player_", text_nums, "_id"),
                             paste0("away_player_", text_nums, "_id")))
  pbp_df
}

#' Calculate PM for each lineup in a single game
#'
#' @param pbp_df the play by play data.frame
#' @return data.frame
plus_minus <- function(pbp_df) {
  require(dplyr)
  pbp_df <- assign_possession_ids(pbp_df)
  pbp_df <- assign_lineup_ids(pbp_df)
  pls_min_df <- pbp_df %>%
    filter(!is.na(lineup_id)) %>%
    group_by(lineup_id) %>%
    summarise(home_possessions = sum(!is.na(possession_team) & possession_team == "HOME"),
              home_points = sum(if_else(!is.na(points) & event_team_id == home_team_id,
                                        points, as.integer(0))),
              home_points_per_poss = home_points / home_possessions,
              away_possessions = sum(!is.na(possession_team) & possession_team == "AWAY"),
              away_points = sum(if_else(!is.na(points) & event_team_id == away_team_id,
                               points, as.integer(0))),
              away_points_per_poss = away_points / away_possessions) %>%
    filter(!(is.nan(home_points_per_poss) & is.nan(away_points_per_poss))) %>%
    mutate(home_points_per_poss = if_else(home_points_per_poss == Inf,
                                          as.double(home_points), home_points_per_poss),
           away_points_per_poss = if_else(away_points_per_poss == Inf,
                                          as.double(away_points), away_points_per_poss))
  avg_rating <- sum(pls_min_df$home_points, pls_min_df$away_points) /
    sum(pls_min_df$away_possessions, pls_min_df$home_possessions)
  pls_min_df <- pls_min_df %>%
    mutate(home_points_per_poss = if_else(is.na(home_points_per_poss),
                                          avg_rating, home_points_per_poss),
           away_points_per_poss = if_else(is.na(away_points_per_poss),
                                          avg_rating, away_points_per_poss))


}


