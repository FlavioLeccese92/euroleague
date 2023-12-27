### ------------------------------------------------------------------------ ###
####------------------------------ PLUS-MINUS ------------------------------####
### ------------------------------------------------------------------------ ###

library(tidyr)
library(dplyr)
library(httr)
library(jsonlite)
library(scales)

### Game ###

# getGameHeader
getGameHeader = function(GameCode, SeasonCode = "E2023"){
  "https://live.euroleague.net/api/Header?" %>% 
    paste0("gamecode=", GameCode, "&seasoncode=", SeasonCode) %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.)  %>%
    return()
}

# getGameBoxScore
getGameBoxScore = function(GameCode, SeasonCode = "E2023"){
  out = "https://live.euroleague.net/api/BoxScore?" %>% 
    paste0("gamecode=", GameCode, "&seasoncode=", SeasonCode, 
           "&temp=", SeasonCode) %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.)
  
  out[["Team"]] = out$Stats$Team
  out[["Coach"]] = out$Stats$Coach
  out[["EndOfQuarter"]] = out[["EndOfQuarter"]] %>% as_tibble()
  out[["ByQuarter"]] = out[["ByQuarter"]] %>% as_tibble()
  out[["PlayerStats"]] = out$Stats$PlayersStats %>% bind_rows() %>% as_tibble()
  out[["TeamStats"]] = out$Stats$totr
  out$Stats = NULL
  
  return(out)
}

# getGamePoints
getGamePoints = function(GameCode, SeasonCode = "E2023"){
  "https://live.euroleague.net/api/Points?" %>% 
    paste0("gamecode=", GameCode, "&seasoncode=", SeasonCode) %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$Rows %>%
    as_tibble() %>% return()
}

# getGameRound
getGameRound = function(GameCode, SeasonCode = "E2023"){
  "https://live.euroleague.net/api/Round?" %>% 
    paste0("gamecode=", GameCode, "&seasoncode=", SeasonCode) %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>%
    return()
}

# getGamePlayers
getGamePlayers = function(GameCode, Team = "VIR", SeasonCode = "E2023"){
  "https://live.euroleague.net/api/Players?" %>% 
    paste0("gamecode=", GameCode, "&seasoncode=", SeasonCode, 
           "&disp=&equipo=", Team, "&temp=", SeasonCode) %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>%
    as_tibble() %>% return()
}

# getGamePlayByPlay
getGamePlayByPlay = function(GameCode, SeasonCode = "E2023"){
  "https://live.euroleague.net/api/PlayByPlay?" %>% 
    paste0("gamecode=", GameCode, "&seasoncode=", SeasonCode, 
           "&disp=&equipo=", Team, "&temp=", SeasonCode) %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% 
  return()
}

# getGameEvolution
getGameEvolution = function(GameCode, SeasonCode = "E2023"){
  "https://live.euroleague.net/api/Evolution?" %>% 
    paste0("gamecode=", GameCode, "&seasoncode=", SeasonCode) %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.)  %>%
    return()
}

### Team ###

# getTeam
getTeam = function(TeamCode = "", Competition = "E", SeasonCode = "E2023"){
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    paste0(Competition, "/seasons/", SeasonCode, "/clubs/", TeamCode) %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    { if(is.null(dim(.)))
      unlist(.) %>% t()%>% 
        as_tibble()
      else as_tibble(.) %>%
        unnest(cols = c(images, country), names_sep = ".")} %>% 
    return()
}

# getTeamPeople
getTeamPeople = function(TeamCode, Competition = "E", SeasonCode = "E2023"){
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    paste0(Competition, "/seasons/", SeasonCode, "/clubs/", TeamCode, "/people") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>%
    as_tibble() %>%
    unnest(cols = c(person, images, club, season), names_sep = ".") %>% 
    unnest(cols = c(person.country, person.birthCountry,
                    person.images, club.images), names_sep = ".") %>% 
    return()
}

# getTeamGames
getTeamGames = function(TeamCode, Competition = "E", SeasonCode = "E2023"){
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    paste0(Competition, "/seasons/", SeasonCode, "/games?TeamCode=", TeamCode) %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    as_tibble() %>%
    unnest(cols = c(season, competition, group, phaseType, round, home,
                    away, venue),
           names_sep = ".") %>% 
    unnest(c(home.quarters, home.coach, home.imageUrls,
             away.quarters, away.coach, away.imageUrls),
           names_sep = ".") %>% select(-broadcasters) %>%
    return()
}

# getTeamStats
getTeamStats = function(TeamCode, Competition = "E", SeasonCode = "E2023",
                        PhaseType = ""){
  out = "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    paste0(Competition, "/seasons/", SeasonCode, "/clubs/", TeamCode,
           "/people/stats?phaseTypeCode=", PhaseType) %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.)
  
  out[["playerStats"]] = out[["playerStats"]] %>% as_tibble() %>%
    {if (nrow(.) > 0) unnest(., cols = c(player, accumulated, averagePerGame),
           names_sep = ".") else NULL }
  
  out[["accumulated"]] = out[["accumulated"]] %>%
    unlist() %>% t() %>% as_tibble()
  out[["averagePerGame"]] = out[["averagePerGame"]] %>%
    unlist() %>% t() %>% as_tibble()
  out[["teamAccumulated"]] = out[["teamAccumulated"]] %>%
    unlist() %>% t() %>% as_tibble()
  out[["teamAveragePerGame"]] = out[["teamAveragePerGame"]] %>%
    unlist() %>% t() %>% as_tibble()
  
  return(out)
}

### Competition ###

# GetCompetitionRounds
GetCompetitionRounds = function(Competition = "E", SeasonCode = "E2023"){
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    paste0(Competition, "/seasons/", SeasonCode, "/rounds") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    as_tibble() %>% 
    return()
}

# getCompetitionGames
getCompetitionGames = function(PhaseType = "", Round, Competition = "E", SeasonCode = "E2023"){
  
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    paste0(Competition, "/seasons/", SeasonCode, "/games?",
           "phaseTypeCode=", PhaseType, "&roundNumber=", Round) %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    as_tibble() %>%
    unnest(cols = c(season, competition, group, phaseType, round, home,
                    away, venue),
           names_sep = ".") %>% 
    unnest(c(home.quarters, home.coach, home.imageUrls,
             away.quarters, away.coach, away.imageUrls),
           names_sep = ".") %>% select(-broadcasters) %>%
    return()
}

# GetCompetitionHistory
GetCompetitionHistory = function(Competition = "E"){
  "https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/" %>% 
    paste0(Competition, "/seasons") %>%
    GET() %>% .$content %>% rawToChar() %>% fromJSON(.) %>% .$data %>%
    as_tibble() %>% 
    return()
}

### Utils ###

# TextContrast
TextContrast = function(hex_color) {
  rgb_color = col2rgb(hex_color)
  if (rgb_color[1] + rgb_color[2] + rgb_color[3] < 380) {
    return("white")
  } else {
    return("black")
  }
}
