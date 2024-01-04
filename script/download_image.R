### ------------------------------------------------------------------------ ###
####------------------------------- DOWNLOADS ------------------------------####
### ------------------------------------------------------------------------ ###

library(dplyr)
library(glue)

TeamAll = getTeam() %>% distinct(TeamCode, ImagesCrest) %>% 
  mutate(ImagesCrest = glue("{ImagesCrest}?width=300"))

for(TeamCodeChosen in TeamAll$TeamCode) {
  
  if (!dir.exists(glue("_temp/{TeamCodeChosen}"))) {dir.create(glue("_temp/{TeamCodeChosen}"))}

  TeamPeople = getTeamPeople(TeamCodeChosen) %>%
    filter(!is.na(ImagesHeadshot)) %>% 
    mutate(Player_ID = trimws(PersonCode),
           ImagesHeadshot = glue("{ImagesHeadshot}?width=300")) %>% 
    distinct(Player_ID, ImagesHeadshot, ClubImagesCrest)
  
  # Download team logo
  
  download.file(unique(TeamPeople$ClubImagesCrest), method = "curl",
                destfile = glue("_temp/{TeamCodeChosen}/{TeamCodeChosen}-logo.png"))
  
  # Download team people images
  
  for (i in 1:nrow(TeamPeople)) {
    download.file(TeamPeople$ImagesHeadshot[i], method = "curl",
                  destfile = glue("_temp/{TeamCodeChosen}/{TeamPeople$Player_ID[i]}.png"))
  }
  
}

for(TeamCodeChosen in TeamAll$TeamCode) {
  FileToRemove = glue("plots/E2023/{TeamCodeChosen}/player_ftg_perc.png")
  if (file.exists(FileToRemove)) { file.remove(FileToRemove) }
}
