### ------------------------------------------------------------------------ ###
####------------------------------ PLUS-MINUS ------------------------------####
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
  "https://live.euroleague.net/api/Header?" %>% 
    glue("gamecode={game_code}&seasoncode={season_code}") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>%
    rename_with(TextFormat) %>%
    return()
}

# getGameBoxScore
getGameBoxScore = function(game_code, season_code = "E2023"){
  getin = "https://live.euroleague.net/api/BoxScore?" %>% 
    glue("gamecode={game_code}&seasoncode={season_code}&temp={season_code}") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.)
  
  out = NULL
  out[["Team"]] = getin$Stats$Team
  out[["Coach"]] = getin$Stats$Coach
  out[["EndOfQuarter"]] = getin[["EndOfQuarter"]] %>% as_tibble() %>% rename_with(TextFormat)
  out[["ByQuarter"]] = getin[["ByQuarter"]] %>% as_tibble() %>% rename_with(TextFormat)
  
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
  "https://live.euroleague.net/api/Points?" %>% 
    glue("gamecode={game_code}&seasoncode={season_code}") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$Rows %>%
    as_tibble() %>%
    rename_with(TextFormat) %>%
    return()
}

# getGameRound
getGameRound = function(game_code, season_code = "E2023"){
  "https://live.euroleague.net/api/Round?" %>% 
    glue("gamecode={game_code}&seasoncode={season_code}") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>%
    rename_with(TextFormat) %>%
    return()
}

# getGamePlayers
getGamePlayers = function(game_code, team_code = "VIR", season_code = "E2023"){
  "https://live.euroleague.net/api/Players?" %>% 
    glue("gamecode={game_code}&seasoncode={season_code}&disp=&equipo={team_code}&temp={season_code}") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>%
    rename_with(TextFormat) %>%
    as_tibble() %>% return()
}

# getGamePlayByPlay
getGamePlayByPlay = function(game_code, season_code = "E2023"){
  "https://live.euroleague.net/api/PlayByPlay?" %>% 
    glue("gamecode={game_code}&seasoncode={season_code}&disp=&equipo={team_code}&temp={season_code}") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% 
    rename_with(TextFormat) %>%
  return()
}

# getGameEvolution
getGameEvolution = function(game_code, season_code = "E2023"){
  "https://live.euroleague.net/api/Evolution?" %>% 
    glue("gamecode={game_code}&seasoncode={season_code}") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>%
    rename_with(TextFormat) %>%
    return()
}

### Team ###

# getTeam
getTeam = function(team_code = "", competition_code = "E", season_code = "E2023"){
  out = NULL
  for (tc in team_code) {
    out = bind_rows(out,
      "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
        glue("{competition_code}/seasons/{season_code}/clubs/{tc}") %>%
        GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
        { if(is.null(dim(.)))
          unlist(.) %>% t() %>% 
            as_tibble()
          else as_tibble(.) %>%
            unnest(cols = c(images, country), names_sep = ".")} %>% 
        rename_with(TextFormat) %>%
        rename(TeamCode = Code, TeamName = Name)
    )
  }
  return(out)
}

# getTeamPeople
getTeamPeople = function(team_code, competition_code = "E", season_code = "E2023"){
  out = NULL
  for (tc in team_code) {
    out = bind_rows(out,
      "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
        glue("{competition_code}/seasons/{season_code}/clubs/{tc}/people") %>%
        GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>%
        as_tibble() %>%
        unnest(cols = c(person, images, club, season), names_sep = ".") %>% 
        unnest(cols = c(person.country, person.birthCountry,
                        person.images, club.images), names_sep = ".") %>% 
        rename_with(TextFormat) %>% 
        mutate(TeamCode = tc,
               PersonCode = trimws(PersonCode),
               Player = paste0(gsub(".*, ", "", PersonName), " ", gsub(",.*", "", PersonName), " #", Dorsal),
               .before = 1) 
    )
  }
  return(out)
}

# getTeamGames
getTeamGames = function(team_code, competition_code = "E", season_code = "E2023"){
  out = NULL
  for (tc in team_code) {
    out = bind_rows(out,
      "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
        glue("{competition_code}/seasons/{season_code}/games?TeamCode={tc}") %>%
        GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
        as_tibble() %>%
        unnest(cols = c(season, competition, group, phaseType, round, home,
                        away, venue),
               names_sep = ".") %>% 
        unnest(c(home.quarters, home.coach, home.imageUrls,
                 away.quarters, away.coach, away.imageUrls),
               names_sep = ".") %>% select(-broadcasters) %>%
        rename_with(TextFormat) %>%
        rename(GameId = Id, GameCode = Code, GameDate = Date, GameStatus = Status, Round = RoundRound) %>% 
        mutate(TeamCode = tc, .before = 1)
    )
  }
  return(out)
}

# getTeamStats
getTeamStats = function(team_code, competition_code = "E", season_code = "E2023", phase_type = ""){
  
  out = NULL
  for (tc in team_code){
    getin = "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
      glue("{competition_code}/seasons/{season_code}/clubs/{tc}/people/stats?phaseTypeCode={phase_type}") %>%
      GET() %>% .$content %>% rawToChar() %>% fromJSON(.)
    
    out[["PlayerAccumulated"]] = bind_rows(
      out[["PlayerAccumulated"]],
      getin[["playerStats"]] %>% as_tibble() %>% 
        select(-averagePerGame) %>%
        unnest(., cols = c(player, accumulated), names_sep = ".") %>%
        rename_with(function(x) {gsub("accumulated\\.", "", x)} ) %>% 
        rename_stat() %>% 
        mutate(TeamCode = tc,
               Player_ID = trimws(PlayerCode), .before = 1, .keep = "unused") %>% 
        mutate(across(contains("%"), ~as.numeric(gsub("%", "", .))))
    )
    out[["PlayerAveragePerGame"]] = bind_rows(
      out[["PlayerAveragePerGame"]],
      getin[["playerStats"]] %>% as_tibble() %>% 
        select(-accumulated) %>%
        unnest(., cols = c(player, averagePerGame), names_sep = ".") %>%
        rename_with(function(x) {gsub("averagePerGame\\.", "", x)}) %>% 
        rename_stat() %>% 
        mutate(TeamCode = tc,
               Player_ID = trimws(PlayerCode), .before = 1, .keep = "unused") %>% 
        mutate(across(contains("%"), ~as.numeric(gsub("%", "", .))))
    )
    out[["TeamAccumulated"]] = bind_rows(
      out[["TeamAccumulated"]],
      getin[["accumulated"]] %>% as_tibble() %>% unnest(cols = everything()) %>%
        mutate(TeamCode = tc, .before = 1) %>% rename_stat() %>% 
        mutate(across(contains("%"), ~as.numeric(gsub("%", "", .))))
    )
    out[["TeamAveragePerGame"]] = bind_rows(
      out[["TeamAveragePerGame"]],
      getin[["averagePerGame"]] %>% as_tibble() %>% unnest(cols = everything()) %>%
        mutate(TeamCode = tc, .before = 1) %>% rename_stat() %>% 
        mutate(across(contains("%"), ~as.numeric(gsub("%", "", .))))
    )
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
"https://feeds.incrowdsports.com/provider/euroleague-feeds/v3/competitions/" %>% 
  glue("{competition_code}/seasons/{season_code}/rounds/{round}/basicstandings") %>%
  GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$teams %>%
  as_tibble()  %>%
  unnest(cols = c(club), names_sep = ".") %>% 
  unnest(c(club.images), names_sep = ".") %>%
  rename_with(TextFormat) %>% 
  return()
}

# GetCompetitionStreaks
GetCompetitionStreaks = function(competition_code = "E", season_code = "E2023", round){
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v3/competitions/" %>% 
    glue("{competition_code}/seasons/{season_code}/rounds/{round}/streaks") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$teams %>%
    as_tibble()  %>%
    unnest(cols = c(club), names_sep = ".") %>% 
    unnest(c(club.images), names_sep = ".") %>%
    rename_with(TextFormat) %>% 
    return()
}
 
# GetCompetitionRounds
GetCompetitionRounds = function(competition_code = "E", season_code = "E2023"){
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    glue("{competition_code}/seasons/{season_code}/rounds") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    as_tibble() %>% 
    rename_with(TextFormat) %>%
    return()
}

# getCompetitionGames
getCompetitionGames = function(phase_type = "", round, competition_code = "E", season_code = "E2023"){
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    glue("{competition_code}/seasons/{season_code}/games?phaseTypeCode={phase_type}&roundNumber={round}") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    as_tibble() %>%
    unnest(cols = c(season, competition, group, phaseType, round, home,
                    away, venue),
           names_sep = ".") %>% 
    unnest(c(home.quarters, home.coach, home.imageUrls,
             away.quarters, away.coach, away.imageUrls),
           names_sep = ".") %>% select(-broadcasters) %>%
    rename_with(TextFormat) %>%
    return()
}

# GetCompetitionHistory
GetCompetitionHistory = function(competition_code = "E"){
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    glue("{competition_code}/seasons") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    as_tibble() %>% 
    rename_with(TextFormat) %>%
    return()
}

### Mixed ###

# GetPlayerAllStats

GetPlayerAllStats = function(team_code) {
  
  GamesPlayed = getTeamGames(team_code) %>% filter(GameStatus == "result") %>%
    mutate(WinLoss = ifelse((TeamCode == HomeCode) == (HomeScore > AwayScore), "Win", "Loss"),
           TeamCodeAgainst = ifelse(TeamCode == HomeCode, AwayCode, HomeCode),
           HomeAway = ifelse((TeamCode == HomeCode), "Home", "Away"), 
           GameDate = as.Date(GameDate),
           TeamScore = ifelse(HomeAway == "Home", HomeScore, AwayScore),
           TeamAgainstScore = ifelse(HomeAway == "Away", HomeScore, AwayScore)) %>% 
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
    rename_with(TextFormat)
  
  return(out)
}

getTeamAllStats = function(team_code) {
  
  GamesPlayed = getTeamGames(team_code) %>% filter(GameStatus == "result") %>%
    mutate(WinLoss = ifelse((TeamCode == HomeCode) == (HomeScore > AwayScore), "Win", "Loss"),
           TeamCodeAgainst = ifelse(TeamCode == HomeCode, AwayCode, HomeCode),
           HomeAway = ifelse((TeamCode == HomeCode), "Home", "Away"), 
           GameDate = as.Date(GameDate),
           TeamScore = ifelse(HomeAway == "Home", HomeScore, AwayScore),
           TeamAgainstScore = ifelse(HomeAway == "Away", HomeScore, AwayScore)) %>% 
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
    rename_with(TextFormat)
    
  
  return(out)
  
}

### Utils ###
rename_stat = function(data) {

  data = data %>% rename_with(TextFormat)
  
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

TextFormat = function(x){
  x %>% 
  gsub("([A-Z])", " \\1", .) %>% 
    gsub("\\.", " ", .) %>% 
    str_to_title() %>% 
    gsub(" ", "", .) %>% 
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
