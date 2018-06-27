get_apm_data = function(season_year) {
  player_data = kingsutils::query_db(sprintf("
                                             WITH most_recent_date AS (
                                             SELECT
                                             player_code,
                                             MAX(measurement_date) AS maxdate
                                             FROM measurement
                                             GROUP BY player_code
                                             ), most_recent_measurement AS (
                                             SELECT
                                             measurement.player_code,
                                             measurement.measurement_date,
                                             measurement.source,
                                             measurement.height_with_shoes AS height,
                                             measurement.weight
                                             FROM measurement
                                             INNER JOIN most_recent_date ON measurement.player_code = most_recent_date.player_code
                                             AND measurement.measurement_date = most_recent_date.maxdate
                                             )
                                             SELECT
                                             player.player_code,
                                             player.first_name_punctuated AS first_name,
                                             player.last_name_punctuated AS last_name,
                                             measurement.height,
                                             measurement.weight,
                                             box_score.season,
                                             box_score.season_type,
                                             box_score.team_code,
                                             box_score.games,
                                             box_score.minutes,
                                             box_score.points,
                                             box_score.two_points_made,
                                             box_score.two_points_attempted,
                                             box_score.three_points_made,
                                             box_score.three_points_attempted,
                                             box_score.free_throws_made,
                                             box_score.free_throws_attempted,
                                             box_score.blocked_shot_attempts,
                                             box_score.offensive_rebounds,
                                             box_score.defensive_rebounds,
                                             box_score.assists,
                                             box_score.turnovers,
                                             box_score.steals,
                                             box_score.blocked_shots,
                                             box_score.personal_fouls,
                                             advanced_box.player_efficiency_rating,
                                             advanced_box.three_point_attempt_rate,
                                             advanced_box.free_throw_rate,
                                             advanced_box.offensive_rebounding_percentage,
                                             advanced_box.defensive_rebounding_percentage,
                                             advanced_box.assist_percentage,
                                             advanced_box.steal_percentage,
                                             advanced_box.block_percentage,
                                             advanced_box.turnover_percentage,
                                             advanced_box.usage_percentage,
                                             advanced_box.win_shares
                                             FROM box_score.nba_player_season AS box_score
                                             INNER JOIN advanced_box_score.nba_player_season AS advanced_box
                                             ON advanced_box.player_code = box_score.player_code
                                             AND advanced_box.season = box_score.season
                                             AND advanced_box.team_code = box_score.team_code
                                             LEFT JOIN most_recent_measurement AS measurement
                                             ON measurement.player_code = box_score.player_code
                                             LEFT JOIN player ON box_score.player_code = player.player_code
                                             WHERE box_score.season = '%s'
                                             ", season_year))
  pbp_data = kingsutils::query_db(sprintf("
                                          SELECT
                                          pbp.game_code,
                                          pbp.game_clock,
                                          pbp.period,
                                          pbp.possession_id,
                                          pbp.pbp_seq,
                                          pbp.possession_team_is_home,
                                          pbp.points_in_possession,
                                          pbp.off_player1_code AS offensive_player_1,
                                          pbp.off_player2_code AS offensive_player_2,
                                          pbp.off_player3_code AS offensive_player_3,
                                          pbp.off_player4_code AS offensive_player_4,
                                          pbp.off_player5_code AS offensive_player_5,
                                          pbp.def_player1_code AS defensive_player_1,
                                          pbp.def_player2_code AS defensive_player_2,
                                          pbp.def_player3_code AS defensive_player_3,
                                          pbp.def_player4_code AS defensive_player_4,
                                          pbp.def_player5_code AS defensive_player_5
                                          FROM pbp.nba AS pbp
                                          INNER JOIN schedule_nba ON pbp.game_code = schedule_nba.game_code
                                          WHERE
                                          schedule_nba.season = %s
                                          AND schedule_nba.season_type = 'Regular Season'
                                          ", season_year))
  list(player = player_data, pbp = pbp_data)
}
apm_data = get_apm_data(2018)
save(apm_data, file = "./data/apm_data.RData")
