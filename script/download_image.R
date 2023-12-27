### ------------------------------------------------------------------------ ###
####------------------------------- DOWNLOADS ------------------------------####
### ------------------------------------------------------------------------ ###

library(dplyr)
library(glue)

TeamAll = getTeam() %>% distinct(code, images.crest) %>% 
  mutate(images.crest = glue("{images.crest}?width=300")) %>%
  rename(team.code.against = code)

for(TeamCodeChosen in TeamAll$team.code.against) {
  
  if (!dir.exists(glue("_temp/{TeamCodeChosen}"))) {dir.create(glue("_temp/{TeamCodeChosen}"))}

  TeamPeople = getTeamPeople(TeamCodeChosen) %>%
    filter(!is.na(images.headshot)) %>% 
    mutate(Player_ID = trimws(person.code),
           images.headshot = glue("{images.headshot}?width=300")) %>% 
    distinct(Player_ID, images.headshot, club.images.crest)
  
  # Download team logo
  
  download.file(unique(TeamPeople$club.images.crest), method = "curl",
                destfile = glue("_temp/{TeamCodeChosen}/{TeamCodeChosen}-logo.png"))
  
  # Download team people images
  
  for (i in 1:nrow(TeamPeople)) {
    download.file(TeamPeople$images.headshot[i], method = "curl",
                  destfile = glue("_temp/{TeamCodeChosen}/{TeamPeople$Player_ID[i]}.png"))
  }
  
}
