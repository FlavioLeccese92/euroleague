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
  out = "https://live.euroleague.net/api/BoxScore?" %>% 
    glue("gamecode={game_code}&seasoncode={season_code}&temp={season_code}") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.)
  
  out[["Team"]] = out$Stats$Team
  out[["Coach"]] = out$Stats$Coach
  out[["EndOfQuarter"]] = out[["EndOfQuarter"]] %>% as_tibble() %>% rename_with(TextFormat)
  out[["ByQuarter"]] = out[["ByQuarter"]] %>% as_tibble() %>% rename_with(TextFormat)
  
  out[["PlayerStats"]] = out$Stats$PlayersStats %>% bind_rows() %>% as_tibble() %>%
    bind_cols(GameCode = game_code) %>% 
    select(GameCode, TeamCode = Team, Player_ID, Player, Dorsal, Minutes, PIR = Valuation, PM = Plusminus, PTS = Points,
           '2FGM' = FieldGoalsMade2, '2FGA' = FieldGoalsAttempted2,
           '3FGM' = FieldGoalsMade3, '3FGA' = FieldGoalsAttempted3,
           'FTM' = FreeThrowsMade, 'FTA' = FreeThrowsAttempted,
           REB = TotalRebounds, OREB = OffensiveRebounds, DREP = DefensiveRebounds,
           AST = Assistances, STL = Steals, BLK = BlocksFavour, TO = Turnovers, 
           FC = FoulsCommited, FR = FoulsReceived) %>% 
    filter(Minutes != "DNP") %>% 
    mutate(Seconds = period_to_seconds(ms(Minutes)), .after = "Minutes",
           Player_ID = trimws(gsub("P", "", Player_ID)),
           .keep = "unused") %>% 
    mutate(`FG%` = 100*((`2FGM` + `3FGM`)/(`2FGA` + `3FGA`)) %>% round(4),
           `2FG%` = 100*(`2FGM`/`2FGA`) %>% round(4),
           `3FG%` = 100*(`3FGM`/`3FGA`) %>% round(4),
           `FTG%` = 100*(`FTM`/`FTA`) %>% round(4)) %>% 
    mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))
  
  out[["TeamStats"]] = out$Stats$totr %>% rename_with(TextFormat)
  out$Stats = NULL
  
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
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    glue("{competition_code}/seasons/{season_code}/clubs/{team_code}") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    { if(is.null(dim(.)))
      unlist(.) %>% t() %>% 
        as_tibble()
      else as_tibble(.) %>%
        unnest(cols = c(images, country), names_sep = ".")} %>% 
    rename_with(TextFormat) %>%
    rename(TeamCode = Code, TeamName = Name) %>%
    return()
}

# getTeamPeople
getTeamPeople = function(team_code, competition_code = "E", season_code = "E2023"){
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    glue("{competition_code}/seasons/{season_code}/clubs/{team_code}/people") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>%
    as_tibble() %>%
    unnest(cols = c(person, images, club, season), names_sep = ".") %>% 
    unnest(cols = c(person.country, person.birthCountry,
                    person.images, club.images), names_sep = ".") %>% 
    rename_with(TextFormat) %>%
    return()
}

# getTeamGames
getTeamGames = function(team_code, competition_code = "E", season_code = "E2023"){
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    glue("{competition_code}/seasons/{season_code}/games?TeamCode={team_code}") %>%
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
    return()
}

# getTeamStats
getTeamStats = function(team_code, competition_code = "E", season_code = "E2023", phase_type = ""){
  out = "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    glue("{competition_code}/seasons/{season_code}clubs/{team_code}/people/stats?phaseTypeCode={phase_type}") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.)
  
  out[["playerStats"]] = out[["playerStats"]] %>% as_tibble() %>%
    {if (nrow(.) > 0) unnest(., cols = c(player, accumulated, averagePerGame),
           names_sep = ".") %>% rename_with(TextFormat) else NULL }
  
  out[["accumulated"]] = out[["accumulated"]] %>%
    unlist() %>% t() %>% as_tibble() %>% rename_with(TextFormat)
  out[["averagePerGame"]] = out[["averagePerGame"]] %>%
    unlist() %>% t() %>% as_tibble() %>% rename_with(TextFormat)
  out[["teamAccumulated"]] = out[["teamAccumulated"]] %>%
    unlist() %>% t() %>% as_tibble() %>% rename_with(TextFormat)
  out[["teamAveragePerGame"]] = out[["teamAveragePerGame"]] %>%
    unlist() %>% t() %>% as_tibble() %>% rename_with(TextFormat)
  
  return(out)
}

### Competition ###

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

GetPlayerAllStats = function(team_code, game_status = "result") {
  
  GamesPlayed = getTeamGames(team_code) %>% filter(GameStatus == game_status) %>%
    mutate(WinLoss = ifelse((team_code == HomeCode) == (HomeScore > AwayScore), "Win", "Loss"),
           TeamCodeAgainst = ifelse(team_code == HomeCode, AwayCode, HomeCode),
           HomeAway = ifelse((team_code == HomeCode), "Home", "Away"), 
           GameDate = as.Date(GameDate),
           TeamScore = ifelse(HomeAway == "Home", HomeScore, AwayScore),
           TeamAgainstScore = ifelse(HomeAway == "Away", HomeScore, AwayScore),
           TeamCode = team_code) %>% 
    distinct(GameCode, Round, GameDate, TeamCode, TeamCodeAgainst, WinLoss, HomeAway, TeamScore, TeamAgainstScore)
  
  out = NULL
  for (i in 1:nrow(GamesPlayed)) {
    out = bind_rows(
      out,
      getGameBoxScore(GamesPlayed$GameCode[i]) %>% .[["PlayerStats"]] %>% filter(TeamCode == team_code)
    )
  }
  
  out = out %>%
    group_by(TeamCode, Player_ID, Player, Dorsal) %>% 
    mutate(across(-c("GameCode", contains("%")), ~sum(., na.rm = TRUE), .names = "G{.col}"),
           GP = n_distinct(GameCode)) %>% 
    mutate(`GFG%` = 100*((`G2FGM` + `G3FGM`)/(`G2FGA` + `G3FGA`)) %>% round(4),
           `G2FG%` = 100*(`G2FGM`/`G2FGA`) %>% round(4),
           `G3FG%` = 100*(`G3FGM`/`G3FGA`) %>% round(4),
           `GFTG%` = 100*(`GFTM`/`GFTA`) %>% round(4),
           GPIR = (GPIR/GP) %>% round(2),
           GPM = (GPM/GP) %>% round(2),
           GPTS = (GPTS/GP) %>% round(2)) %>% 
    ungroup() %>% 
    mutate(across(everything(), ~ifelse(is.nan(.), NA, .))) %>% 
    left_join(GamesPlayed, by = c("TeamCode", "GameCode")) %>%
    rename_with(TextFormat)
  
  return(out)
}

### Utils ###

StatsRange = tibble(
  Stat = c("PM", "FG%", "3FG%", "2FG%", "FTG%", "PTS", "PIR"),
  Min = c(-30, 0, 0, 0, 0, 0, 0),
  Max = c(30, 100, 100, 100, 100, 40, 50),
  By = c(15, 20, 20, 20, 20, 10, 10),
  TopMargin = c(10, 15, 15, 15, 15, 15, 15),
  BottomMargin = c(10, 10, 10, 10, 10, 10, 10),
  MiddleOffset = c(15, 0, 0, 0, 0, 0, 0),
  BottomOffset = c(0, 10, 10, 10, 10, 10, 10),
  Unit = c("", "%", "%", "%", "%", "", "")
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
