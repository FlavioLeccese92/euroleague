tibble(
  Endpoint = c("https://live.euroleague.net/api/Header",
               "https://live.euroleague.net/api/BoxScore",
               "https://live.euroleague.net/api/Points",
               "https://live.euroleague.net/api/Round",
               "https://live.euroleague.net/api/Players",
  ),
  Function = c("getGameHeader",
               "getGameBoxScore",
               "getGamePoints",
               "getGameRound",
               "getGamePlayers"
               ),
  Primitive = c(TRUE,
                TRUE,
                TRUE,
                TRUE,
                TRUE,
                
                )
  Output = c("1dim tibble",
             "list of tibbles",
             "2dim tibble",
             "Character",
             "2dim tibble",
             
             )
)
