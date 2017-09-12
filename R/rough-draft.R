# figure out best way to calculate plus-minus
data(pbp)
library(dplyr)
game_df <- pbp %>%
  filter(points > 0) %>%
  arrange(period_sequence, desc(clock)) %>%
  select(home_team_id, away_team_id, event_team_id,
         matches("(home|away)(.*)id"), points) %>%
  mutate(points = as.numeric(points))
home_players <- game_df %>%
  select(matches("home_player"))
home_players <- t(apply(home_players, 1, sort))
game_df <- game_df %>%
  mutate(home_player_one_id = home_players[, 1],
         home_player_two_id = home_players[, 2],
         home_player_three_id = home_players[, 3],
         home_player_four_id = home_players[, 4],
         home_player_five_id = home_players[, 5])
away_players <- game_df %>%
  select(matches("away_player"))
away_players <- t(apply(away_players, 1, sort))
game_df <- game_df %>%
  mutate(away_player_one_id = away_players[, 1],
         away_player_two_id = away_players[, 2],
         away_player_three_id = away_players[, 3],
         away_player_four_id = away_players[, 4],
         away_player_five_id = away_players[, 5])
lineup_df <- game_df %>%
  select(matches("(home|away)(.*)id")) %>%
  unique() %>%
  na.omit()

lineup_df <- lineup_df %>% select(matches("player"))

ulineups <- lineup_df %>%
  select(matches("^player_(.*)")) %>%
  duplicated()
lineup_df <- lineup_df %>%
  filter(!ulineups) %>%
  mutate(lineup_id = 1:n())

plus_minus_df <- left_join(game_df, lineup_df, by = ) %>%
  mutate(plus_minus = ifelse(event_team_id == home_team_id, points, -1*points)) %>%
  group_by(lineup_id) %>%
  select(lineup_id, plus_minus) %>%
  summarise(plus_minus = sum(plus_minus))

pbp <- query_db("select *
                from sportradar_play_by_play
                where game_id = any(array(select game_id
                  from sportradar_schedule
                  where season_year = 2017
                  and season_type = 'REG'
                  and league_alias = 'NBA'))")
schedule <- query_db("select * from sportradar_schedule
                     where season_year = 2017
                     and season_type = 'REG'
                     and league_alias = 'NBA'")

postgres_array(players_vec) -> players_array
player_profiles <- query_db(sprintf("select * from sportradar_player_profile
                                    where player_id = any(%s)
                                    and season_year = 2017", players_array))
