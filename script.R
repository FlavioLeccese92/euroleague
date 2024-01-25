### ------------------------------------------------------------------------ ###
####-------------------------------- SCRIPT --------------------------------####
### ------------------------------------------------------------------------ ###

library(tidyr)
library(dplyr)
library(httr)
library(jsonlite)
library(scales)
library(glue)
library(lubridate)
library(stringr)

### Game ###

# getGameHeader
getGameHeader = function(game_code, season_code = "E2023"){
  GET("https://live.euroleague.net/api/Header?",
      query = list(gamecode = game_code,
                   seasoncode = season_code)) %>% 
    .$content %>% rawToChar() %>% fromJSON(.) %>% as_tibble() %>% 
    rename_with(TextFormatType1) %>%
    return()
}

# getGameBoxScore
getGameBoxScore = function(game_code, season_code = "E2023"){
  getin = GET("https://live.euroleague.net/api/BoxScore",
              query = list(gamecode = game_code,
                           seasoncode = season_code)) %>% 
    .$content %>% rawToChar() %>% fromJSON(.)
  
  out = NULL
  out[["Team"]] = getin$Stats$Team
  out[["Coach"]] = getin$Stats$Coach
  out[["EndOfQuarter"]] = getin[["EndOfQuarter"]] %>% as_tibble() %>% rename_with(TextFormatType1)
  out[["ByQuarter"]] = getin[["ByQuarter"]] %>% as_tibble() %>% rename_with(TextFormatType1)
  
  out[["PlayerStats"]] = getin$Stats$PlayersStats %>% bind_rows() %>% as_tibble() %>%
    bind_cols(GameCode = game_code, .) %>% 
    rename(TeamCode = Team) %>% 
    rename_stat() %>% 
    filter(Minutes != "DNP") %>% 
    mutate(Player = paste0(gsub(".*, ", "", Player), " ", gsub(",.*", "", Player), " #", Dorsal),
           Seconds = period_to_seconds(ms(Minutes)), .after = "Minutes",
           Player_ID = trimws(gsub("P", "", Player_ID)),
           .keep = "unused") %>%
    mutate(`FG%` = 100*((`2FGM` + `3FGM`)/(`2FGA` + `3FGA`)) %>% round(4),
           `2FG%` = 100*(`2FGM`/`2FGA`) %>% round(4),
           `3FG%` = 100*(`3FGM`/`3FGA`) %>% round(4),
           `FT%` = 100*(`FTM`/`FTA`) %>% round(4)) %>% 
    mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))
  
  out[["TeamStats"]] = getin$Stats$totr %>% as_tibble() %>% 
    bind_cols(TeamCode = unique(out[["PlayerStats"]]$TeamCode), .) %>%
    bind_cols(GameCode = game_code, .) %>% rename_stat() %>% 
    mutate(Seconds = period_to_seconds(ms(Minutes)), .after = "Minutes",
           .keep = "unused") %>%
    mutate(`FG%` = 100*((`2FGM` + `3FGM`)/(`2FGA` + `3FGA`)) %>% round(4),
           `2FG%` = 100*(`2FGM`/`2FGA`) %>% round(4),
           `3FG%` = 100*(`3FGM`/`3FGA`) %>% round(4),
           `FT%` = 100*(`FTM`/`FTA`) %>% round(4)) %>% 
    mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))
  
  return(out)
}

# getGamePoints
getGamePoints = function(game_code, season_code = "E2023"){
  GET("https://live.euroleague.net/api/Points",
      query = list(gamecode = game_code,
                   seasoncode = season_code)) %>%
    .$content %>% rawToChar() %>% fromJSON(.) %>% .$Rows %>%
    as_tibble() %>%
    rename_with(TextFormatType2) %>%
    mutate(Player_ID = trimws(gsub("P", "", Player_ID)),
           Utc = as.POSIXct(Utc, format = "%Y%m%d%H%M%OS", tz = "UTC")) %>%
    mutate(GameCode = game_code, 
           TeamCode = trimws(Team), .keep = "unused", .before = 1) %>% 
    return()
}

# getGameRound
getGameRound = function(game_code, season_code = "E2023"){
  GET("https://live.euroleague.net/api/Round",
      query = list(gamecode = game_code,
                   seasoncode = season_code)) %>%
    .$content %>% rawToChar() %>% fromJSON(.) %>%
    return()
}

# getGamePlayers
getGamePlayers = function(game_code, team_code = "VIR", season_code = "E2023"){
  GET("https://live.euroleague.net/api/Players",
      query = list(gamecode = game_code,
                   seasoncode = season_code,
                   equipo = team_code,
                   temp = season_code)) %>%
    .$content %>% rawToChar() %>% fromJSON(.) %>% as_tibble() %>% 
    rename_with(TextFormatType1) %>%
    return()
}

# getGamePlayByPlay
getGamePlayByPlay = function(game_code, season_code = "E2023"){
  getin = GET("https://live.euroleague.net/api/PlayByPlay",
              query = list(gamecode = game_code,
                           seasoncode = season_code)) %>%
    .$content %>% rawToChar() %>% fromJSON(.)
  
  out = NULL
  out[["PlayByPlaySummary"]] = getin[c("Live", "TeamA", "TeamB",
                                       "CodeTeamA", "CodeTeamB",
                                       "ActualQuarter")] %>%
    bind_rows() %>%
    mutate(across(everything(), trimws))
  
  out[["PlayByPlay"]] = bind_rows(
    getin[["FirstQuarter"]] %>% {if (length(.) > 0) mutate(., Quarter = 1, .before = 1) else NULL},
    getin[["SecondQuarter"]] %>% {if (length(.) > 0) mutate(., Quarter = 2, .before = 1) else NULL}, 
    getin[["ThirdQuarter"]] %>% {if (length(.) > 0) mutate(., Quarter = 3, .before = 1) else NULL},
    getin[["ForthQuarter"]] %>% {if (length(.) > 0) mutate(., Quarter = 4, .before = 1) else NULL},
    getin[["ExtraTime"]] %>% {if (length(.) > 0) mutate(., Quarter = 5, .before = 1) else NULL} ) %>%
      as_tibble() %>% 
    rename_with(TextFormatType3) %>% 
    mutate(across(where(is.character), trimws),
           across(everything(), ~ifelse(nchar(.) == 0, NA, .)))
  
  return(out)
}

# getGameEvolution
getGameEvolution = function(game_code, season_code = "E2023"){
  getin = GET("https://live.euroleague.net/api/Evolution",
              query = list(gamecode = game_code,
                           seasoncode = season_code)) %>%
    .$content %>% rawToChar() %>% fromJSON(.)
  
  out = NULL
  out[["EvolutionSummary"]] = getin[c("MinuteMaxA", "MinuteMaxB", 
                                      "ScoreMaxA", "ScoreMaxB",
                                      "difp", "dA", "dB")] %>%
    bind_rows() %>%
    mutate(across(everything(), trimws))
  
  out[["Evolution"]] = bind_cols(
    getin[["MinutesList"]] %>% as_tibble() %>% rename(Minute = value),
    getin[["PointsList"]] %>% t() %>% as_tibble() %>% rename(PointsTeamA = V1, PointsTeamB = V2),
    getin[["ScoreDiffPerMinute"]] %>% t() %>% as_tibble() %>% rename(DiffTeamA = V1, DiffTeamB = V2) %>% 
      mutate(DiffTeamB = abs(DiffTeamB))
  )
  return(out)
}

### Team ###

# getTeam
getTeam_single = function(team_code = "", competition_code = "E", season_code = "E2023", ...){
  GET(glue("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/",
           "competitions/{competition_code}/seasons/{season_code}/clubs/{team_code}")) %>%
    .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    { if (is.null(dim(.)))
      unlist(.) %>% t() %>% as_tibble()
      else as_tibble(.) %>%
        unnest(cols = c(images, country), names_sep = ".")} %>% 
    rename_with(TextFormatType1) %>%
    rename(TeamCode = Code, TeamName = Name) %>% 
    return()
}

getTeam = function(team_code = "", competition_code = "E", season_code = "E2023"){
  out = NULL
  for (tc in team_code) {
    out = bind_rows(
      out,
      getTeam_single(tc, competition_code, season_code)
    )
  }
  return(out)
}

# getTeamPeople
getTeamPeople_single = function(team_code, competition_code = "E", season_code = "E2023", ...){

  GET(glue("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/",
           "competitions/{competition_code}/seasons/{season_code}/clubs/{team_code}/people")) %>%
    .$content %>% rawToChar() %>% fromJSON(.) %>%
    as_tibble() %>%
    unnest(cols = c(person, images, club, season), names_sep = ".") %>% 
    unnest(cols = c(person.country, person.birthCountry,
                    person.images, club.images), names_sep = ".") %>% 
    rename_with(TextFormatType1) %>% 
    mutate(TeamCode = team_code,
           PersonCode = trimws(PersonCode),
           Player = paste0(gsub(".*, ", "", PersonName), " ", gsub(",.*", "", PersonName), " #", Dorsal),
           .before = 1) %>%
    return()
}

getTeamPeople = function(team_code = "", competition_code = "E", season_code = "E2023"){
  out = NULL
  for (tc in team_code) {
    out = bind_rows(
      out,
      getTeamPeople_single(tc, competition_code, season_code)
    )
  }
  return(out)
}

# getTeamGames
getTeamGames_single = function(team_code, competition_code = "E", season_code = "E2023", ...){
  
  GET(glue("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/",
           "competitions/{competition_code}/seasons/{season_code}/games"),
      query = list(TeamCode = team_code)) %>% 
    .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    as_tibble() %>%
    unnest(cols = c(season, competition, group, phaseType, round, home,
                    away, venue),
           names_sep = ".") %>% 
    unnest(c(home.quarters, home.coach, home.imageUrls,
             away.quarters, away.coach, away.imageUrls),
           names_sep = ".") %>% select(-broadcasters) %>%
    rename_with(TextFormatType1) %>%
    rename(GameId = Id, GameCode = Code, GameDate = Date, GameStatus = Status, Round = RoundRound) %>% 
    mutate(TeamCode = team_code,
           WinLoss = ifelse((TeamCode == HomeCode) == (HomeScore > AwayScore), "Win", "Loss"),
           TeamCodeAgainst = ifelse(TeamCode == HomeCode, AwayCode, HomeCode),
           HomeAway = ifelse((TeamCode == HomeCode), "Home", "Away"), 
           GameDate = as.Date(GameDate),
           TeamScore = ifelse(HomeAway == "Home", HomeScore, AwayScore),
           TeamAgainstScore = ifelse(HomeAway == "Away", HomeScore, AwayScore), .before = 1) %>% 
    return()
}

getTeamGames = function(team_code = "", competition_code = "E", season_code = "E2023"){
  out = NULL
  for (tc in team_code) {
    out = bind_rows(
      out,
      getTeamGames_single(tc, competition_code, season_code)
    )
  }
  return(out)
}

# lapply(team_code, function(tc) { getTeamGames_single(tc, competition_code = "E", season_code = "E2023") }) %>% 
#   bind_rows()

getTeamStats_single = function(team_code, competition_code = "E", season_code = "E2023", phase_type = "", ...){
  
  getin = GET(glue("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/",
                   "competitions/{competition_code}/seasons/{season_code}/clubs/{team_code}/people/stats"),
              query = list(phaseTypeCode = phase_type)) %>% 
    .$content %>% rawToChar() %>% fromJSON(.)
  
  out = NULL
  out[["PlayerAccumulated"]] = getin[["playerStats"]] %>% 
    as_tibble() %>% 
    select(-averagePerGame) %>%
    unnest(., cols = c(player, accumulated), names_sep = ".") %>%
    rename_with(function(x) {gsub("accumulated\\.", "", x)} ) %>% 
    rename_stat() %>% 
    mutate(TeamCode = team_code,
           Player_ID = trimws(PlayerCode), .before = 1, .keep = "unused") %>% 
    mutate(across(contains("%"), ~as.numeric(gsub("%", "", .))))
  
  out[["PlayerAveragePerGame"]] = getin[["playerStats"]] %>% 
    as_tibble() %>% 
    select(-accumulated) %>%
    unnest(., cols = c(player, averagePerGame), names_sep = ".") %>%
    rename_with(function(x) {gsub("averagePerGame\\.", "", x)}) %>% 
    rename_stat() %>% 
    mutate(TeamCode = team_code,
           Player_ID = trimws(PlayerCode), .before = 1, .keep = "unused") %>% 
    mutate(across(contains("%"), ~as.numeric(gsub("%", "", .))))
  
  out[["PlayerAveragePer40"]] = out[["PlayerAccumulated"]] %>%
    mutate(across(-c("TeamCode", contains("Player"), contains("%")),
                  ~ round(40*60*./TimePlayed, 2)))
  
  out[["TeamAccumulated"]] = getin[["accumulated"]] %>% 
    as_tibble() %>% unnest(cols = everything()) %>%
    mutate(TeamCode = team_code, .before = 1) %>% rename_stat() %>% 
    mutate(across(contains("%"), ~as.numeric(gsub("%", "", .))))
  
  out[["TeamAveragePerGame"]] = getin[["averagePerGame"]] %>%
    as_tibble() %>% unnest(cols = everything()) %>%
    mutate(TeamCode = team_code, .before = 1) %>% rename_stat() %>% 
    mutate(across(contains("%"), ~as.numeric(gsub("%", "", .))))
  
  return(out)
}

getTeamStats = function(team_code, competition_code = "E", season_code = "E2023", phase_type = ""){
    
  out = NULL
  for (tc in team_code) {
    getin = getTeamStats_single(tc)
    
    out[["PlayerAccumulated"]] = bind_rows(
      out[["PlayerAccumulated"]],
      getin[["PlayerAccumulated"]])
    
    out[["PlayerAveragePerGame"]] = bind_rows(
      out[["PlayerAveragePerGame"]],
      getin[["PlayerAveragePerGame"]])
    
    out[["TeamAccumulated"]] = bind_rows(
      out[["TeamAccumulated"]],
      getin[["TeamAccumulated"]])
    
    out[["TeamAveragePerGame"]] = bind_rows(
      out[["TeamAveragePerGame"]],
      getin[["TeamAveragePerGame"]])
  }
  
  out[["PlayerAveragePer40"]] = 
    out[["PlayerAccumulated"]] %>%
    mutate(across(-c("TeamCode", contains("Player"), contains("%")),
                  ~ round(40*60*./TimePlayed, 2)))
  return(out)
}

### Competition ###

# GetCompetitionStandings
GetCompetitionStandings = function(competition_code = "E", season_code = "E2023", round){
  GET(glue("https://feeds.incrowdsports.com/provider/euroleague-feeds/v3/",
           "competitions/{competition_code}/seasons/{season_code}/rounds/{round}/basicstandings")) %>%
    .$content %>% rawToChar() %>% fromJSON(.) %>% .$teams %>%
    as_tibble()  %>%
    unnest(cols = c(club), names_sep = ".") %>% 
    unnest(c(club.images), names_sep = ".") %>%
    rename_with(TextFormatType1) %>% 
    return()
}

# GetCompetitionStreaks
GetCompetitionStreaks = function(competition_code = "E", season_code = "E2023", round){
  GET(glue("https://feeds.incrowdsports.com/provider/euroleague-feeds/v3/",
           "competitions/{competition_code}/seasons/{season_code}/rounds/{round}/streaks")) %>%
    .$content %>% rawToChar() %>% fromJSON(.) %>% .$teams %>%
    as_tibble()  %>%
    unnest(cols = c(club), names_sep = ".") %>% 
    unnest(c(club.images), names_sep = ".") %>%
    rename_with(TextFormatType1) %>% 
    return()
}
 
# GetCompetitionRounds
GetCompetitionRounds = function(competition_code = "E", season_code = "E2023"){
  GET(glue("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/",
           "competitions/{competition_code}/seasons/{season_code}/rounds")) %>%
    .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    as_tibble() %>% 
    rename_with(TextFormatType1) %>%
    return()
}

# getCompetitionGames
getCompetitionGames = function(phase_type = "", round, competition_code = "E", season_code = "E2023"){
  GET(glue("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/",
           "competitions/{competition_code}/seasons/{season_code}/games"),
      query = list(phaseTypeCode = phase_type,
                   roundNumber = round)) %>%
    .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    as_tibble() %>%
    unnest(cols = c(season, competition, group, phaseType, round, home,
                    away, venue),
           names_sep = ".") %>% 
    unnest(c(home.quarters, home.coach, home.imageUrls,
             away.quarters, away.coach, away.imageUrls),
           names_sep = ".") %>% select(-broadcasters) %>%
    rename_with(TextFormatType1) %>%
    return()
}

# GetCompetitionHistory
GetCompetitionHistory = function(competition_code = "E"){
  GET(glue("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/",
           "competitions/{competition_code}/seasons")) %>%
    .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    as_tibble() %>% 
    rename_with(TextFormatType1) %>%
    return()
}


# GetPlayerAllStats

GetPlayerAllStats = function(team_code) {
  
  GamesPlayed = getTeamGames(team_code) %>% filter(GameStatus == "result") %>% 
    distinct(GameCode, Round, GameDate, TeamCode, TeamCodeAgainst, WinLoss, HomeAway, TeamScore, TeamAgainstScore)
  
  out = NULL
  for (gp in unique(GamesPlayed$GameCode)) {
    out = bind_rows(
      out,
      getGameBoxScore(gp) %>% .[["PlayerStats"]] %>% filter(TeamCode %in% unique(GamesPlayed$TeamCode))
    )
  }
  
 out = out %>%
    group_by(TeamCode, Player_ID, Player) %>% 
    mutate(across(-c("GameCode", contains("%")), ~sum(., na.rm = TRUE), .names = "G{.col}"),
           GP = n_distinct(GameCode)) %>% 
    mutate(`GFG%` = 100*(`G2FGM` + `G3FGM`)/(`G2FGA` + `G3FGA`),
           `G2FG%` = 100*`G2FGM`/`G2FGA`,
           `G3FG%` = 100*`G3FGM`/`G3FGA`,
           `GFT%` = 100*`GFTM`/`GFTA`) %>% 
    ungroup() %>% 
    mutate(across(-c("GameCode", "GP", ends_with("%")) & starts_with("G"), ~round(./GP, 2)),
           across(ends_with("%"), ~round(., 2)),
           across(everything(), ~ifelse(is.nan(.), NA, .))) %>% 
    left_join(GamesPlayed, by = c("TeamCode", "GameCode")) %>%
    rename_with(TextFormatType1)
  
  return(out)
}

getTeamAllStats = function(team_code) {
  
  GamesPlayed = getTeamGames(team_code) %>% filter(GameStatus == "result") %>% 
    distinct(GameCode, Round, GameDate, TeamCode, TeamCodeAgainst, WinLoss, HomeAway, TeamScore, TeamAgainstScore)
  
  out = NULL
  for (gp in unique(GamesPlayed$GameCode)) {
    out = bind_rows(
      out,
      getGameBoxScore(gp) %>% .[["TeamStats"]] %>% filter(TeamCode %in% unique(GamesPlayed$TeamCode))
    )
  }
  
  out = out %>%
    left_join(GamesPlayed %>% distinct(GameCode, TeamCode, HomeAway, WinLoss), by = c("GameCode", "TeamCode")) %>% 
    relocate(HomeAway, WinLoss, .after = TeamCode) %>%
    group_by(TeamCode, HomeAway, WinLoss) %>% 
    mutate(across(-c("GameCode", contains("%")), ~sum(., na.rm = TRUE), .names = "G{.col}"),
           GP = n_distinct(GameCode)) %>% 
    mutate(`GFG%` = 100*(`G2FGM` + `G3FGM`)/(`G2FGA` + `G3FGA`),
           `G2FG%` = 100*`G2FGM`/`G2FGA`,
           `G3FG%` = 100*`G3FGM`/`G3FGA`,
           `GFT%` = 100*`GFTM`/`GFTA`) %>% 
    ungroup() %>% 
    mutate(across(-c("GameCode", "GP", ends_with("%")) & starts_with("G"), ~round(./GP, 2)),
           across(ends_with("%"), ~round(., 2)),
           across(everything(), ~ifelse(is.nan(.), NA, .))) %>%
    rename_with(TextFormatType1)
  
  return(out)
}

getTeamAllStatsAgainst = function(team_code) {
  
  GamesPlayed = getTeamGames(team_code) %>% filter(GameStatus == "result") %>% 
    distinct(GameCode, Round, GameDate, TeamCode, TeamCodeAgainst, WinLoss, HomeAway, TeamScore, TeamAgainstScore)
  
  out = NULL
  for (gp in unique(GamesPlayed$GameCode)) {
    out = bind_rows(
      out,
      getGameBoxScore(gp) %>% .[["TeamStats"]] %>%
        bind_cols(TeamCodeAgainst = rev(.$TeamCode), .) %>%
        filter(TeamCodeAgainst %in% unique(GamesPlayed$TeamCode)) %>% 
        mutate(TeamCode = TeamCodeAgainst, .after = "GameCode", .keep = "unused")
    )
  }
  
  out = out %>%
    left_join(GamesPlayed %>% distinct(GameCode, TeamCode, HomeAway, WinLoss), by = c("GameCode", "TeamCode")) %>% 
    relocate(HomeAway, WinLoss, .after = TeamCode) %>%
    group_by(TeamCode, HomeAway, WinLoss) %>% 
    mutate(across(-c("GameCode", contains("%")), ~sum(., na.rm = TRUE), .names = "G{.col}"),
           GP = n_distinct(GameCode)) %>% 
    mutate(`GFG%` = 100*(`G2FGM` + `G3FGM`)/(`G2FGA` + `G3FGA`),
           `G2FG%` = 100*`G2FGM`/`G2FGA`,
           `G3FG%` = 100*`G3FGM`/`G3FGA`,
           `GFT%` = 100*`GFTM`/`GFTA`) %>% 
    ungroup() %>% 
    mutate(across(-c("GameCode", "GP", ends_with("%")) & starts_with("G"), ~round(./GP, 2)),
           across(ends_with("%"), ~round(., 2)),
           across(everything(), ~ifelse(is.nan(.), NA, .))) %>%
    rename_with(TextFormatType1)
  
  return(out)
  
}

getGamePointsTeam = function(team_code) {
  GamesPlayed = getTeamGames(team_code) %>% filter(GameStatus == "result") %>% 
    distinct(GameCode, Round, GameDate, TeamCode, TeamCodeAgainst, WinLoss, HomeAway, TeamScore, TeamAgainstScore)
  
  out = NULL
  for (gp in unique(GamesPlayed$GameCode)) {
    out = bind_rows(
      out,
      getGamePoints(gp) %>% filter(TeamCode %in% unique(GamesPlayed$TeamCode))
    )
  }
  
  return(out)
}

### Utils ###
rename_stat = function(data) {

  data = data %>% rename_with(TextFormatType1)
  
  exchange_table = tibble(
    col_to = c("PIR", "PM", "PTS", "2FGM", "2FGA", "3FGM", "3FGA", 
               "FTM", "FTA", "FGM", "FGA",
               "REB", "OREB", "DREB", "AST", "STL", 
               "BLK", "BLKA", "TO", "FC", "FR", "2FG%", "3FG%", "FT%",
               "GP", "AM", "AA"),
    col_from = c("valuation", "plusminus", "points", "fieldgoalsmade2", 
                 "fieldgoalsattempted2", "fieldgoalsmade3", "fieldgoalsattempted3", 
                 "freethrowsmade", "freethrowsattempted", 
                 "fieldgoalsmadetotal", "fieldgoalsattemptedtotal",
                 "totalrebounds", "offensiverebounds", "defensiverebounds", "assistances", 
                 "steals", "blocksfavour", "blocksagainst", "turnovers", "foulscommited",
                 "foulsreceived", "twopointshootingpercentage",
                 "threepointshootingpercentage", "freethrowshootingpercentage",
                 "gamesplayed", "accuracymade", "accuracyattempted")
  )
  
  names(data) = tibble(col_data = names(data)) %>% 
    mutate(col_data_lower = col_data %>% tolower()) %>%
    left_join(exchange_table, by = c("col_data_lower" = "col_from")) %>% 
    mutate(col_to = col_to %>% ifelse(is.na(.), col_data, .)) %>% 
    pull(col_to)
  
  return(data)
}

StatsRange = tibble(
  Stat = c("PM", "FG%", "3FG%", "2FG%", "FT%", "PTS", "PIR"),
  Min = c(-30, 0, 0, 0, 0, 0, 0),
  Max = c(30, 100, 100, 100, 100, 40, 50),
  GMin = c(-15, 0, 0, 0, 0, 0, 0),
  GMax = c(15, 100, 100, 100, 100, 30, 30),
  By = c(15, 20, 20, 20, 20, 10, 10),
  TopMargin = c(10, 15, 15, 15, 15, 15, 15),
  BottomMargin = c(10, 10, 10, 10, 10, 10, 10),
  MiddleOffset = c(15, 0, 0, 0, 0, 0, 0),
  BottomOffset = c(0, 10, 10, 10, 10, 10, 10),
  Unit = c("", "%", "%", "%", "%", "", ""),
  Name = c("Plus-minus (PM)", "Total field goal % (FG%)", "3-points field goal % (3FG%)",
           "2-points field goal % (2FG%)", "Free-throw % (FT%)", "Total points made (PTS)",
           "Valuation (PIR)")
)

TextFormatType1 = function(x){
  x %>% 
  gsub("([A-Z])", " \\1", .) %>% 
    gsub("\\.", " ", .) %>% 
    str_to_title() %>% 
    gsub(" ", "", .) %>% 
    return()
}
TextFormatType2 = function(x){
  x %>% 
    gsub("_", " ", .) %>% 
    str_to_title(.) %>% 
    gsub(" ", "", .) %>% 
    gsub("IdPlayer", "Player_ID", .) %>% 
    gsub("IdAction", "Action_ID", .) %>% 
  return()
}
TextFormatType3 = function(x){
  case_when(
    x == "PLAYER_ID" ~ "IdPlayer",
    x == "NUMBEROFPLAY" ~ "NumberOfPlay",
    x == "CODETEAM" ~ "CodeTeam",
    x == "PLAYINFO" ~ "PlayInfo",
    x == "PLAYTYPE" ~ "PlayType",
    x == "POINTS_A" ~ "PointsA",
    x == "POINTS_B" ~ "PointsB",
    TRUE ~ str_to_title(x)) %>% 
    return()
}

# TextContrast
TextContrast = function(hex_color) {
  rgb_color = col2rgb(hex_color)
  if (rgb_color[1] + rgb_color[2] + rgb_color[3] < 380) {
    return("white")
  } else {
    return("black")
  }
}

# ConstructCourt
# Taken from https://github.com/solmos/eurolig/blob/5a6e10ca793649a570b76db813f8d9c533cb3904/R/plotShotchart.R

ConstructCourt = function() {
  outer_lines = tibble(
    x = c(-7.5, -7.5, 7.5, 7.5, -7.5),
    y = c(0, 14, 14, 0, 0),
    type = "Outer lines"
  )
  
  paint = tibble(
    x = c(-2.45, -2.45, 2.45, 2.45),
    y = c(0, 5.8, 5.8, 0),
    type = "Paint"
  )
  
  ft_circle = tibble(
    ConstructArc(x0 = 0, y0 = 5.8, r = 1.8, start = 0, stop = pi),
    type = "FT circle"
  )
  
  # The 3pt line transforms into a straight line in the corners
  # Precisely, it transforms to a vertical line when the x coordinates
  # of the arc are above or below 6.6 and -6.6 respectively.
  upper_arc3 = tibble(
    ConstructArc(x0 = 0, y0 = 1.575, r = 6.75, start = 0, stop = pi),
    type = "Upper arc"
  ) %>% filter(abs(.data$x) <= 6.6)
  
  # To find the y coordinate where the vertical line in the corner and
  # the 3pt arc meet, we just find the minimum value of the arc above
  y_max_corner = min(upper_arc3$y)
  left_corner3 = tibble(
    x = c(-6.6, -6.6),
    y = c(0, y_max_corner),
    type = "Left corner 3"
  )
  right_corner3 = tibble(
    x = c(6.6, 6.6),
    y = c(y_max_corner, 0),
    type = "Right corner 3"
  )
  arc3 = rbind(right_corner3, upper_arc3, left_corner3)
  
  backboard = tibble(
    x = c(-0.9, 0.9),
    y = c(1.2, 1.2),
    type = "backboard"
  )
  
  rim = ConstructArc(x0 = 0, y0 = 1.575, r = 0.225,
                       start = 0, stop = 2 * pi) %>% 
    cbind(type = "rim") %>% 
    as_tibble()
  
  semi_circle = tibble(
    ConstructArc(0, 1.575, r = 1.25, 0, pi),
    type = "semi_circle"
  )
  semi_circle_left = tibble(
    x = c(-1.25, -1.25),
    y = c(1.575, 1.2),
    type = "semi_circle_left"
  )
  semi_circle_right = tibble(
    x = c(1.25, 1.25),
    y = c(1.575, 1.2),
    type = "semi_circle_right"
  )
  restricted_area = rbind(semi_circle_right, semi_circle, semi_circle_left)
  
  # middle_circle = tibble(
  #   ConstructArc(0, 14, 1.8, pi, 2 * pi),
  #   type = "middle_circle"
  # )
    
  Court = bind_rows(
    outer_lines,
    paint,
    ft_circle,
    arc3,
    backboard,
    rim,
    restricted_area#,
    # middle_circle
    )
  return(Court)
}


ConstructArc = function(x0, y0, r, start, stop) {
  by = ifelse(start <= stop, 0.001, -0.001)
  theta = seq(start, stop, by)
  x = x0 + r * cos(theta)
  y = y0 + r * sin(theta)
  
  return(tibble(x, y))
}
