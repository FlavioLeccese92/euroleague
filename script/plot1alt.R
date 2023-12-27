Sys.setlocale(locale = "es_ES.UTF-8")

EuroleagueLogo = "https://media-cdn.incrowdsports.com/23610a1b-1c2e-4d2a-8fe4-ac2f8e400632.svg"
TeamCodeChosen = "BAS"

TeamAll = getTeam() %>% distinct(code, images.crest) %>% 
  mutate(images.crest = paste0(images.crest, "?width=75")) %>%
  rename(team.code.against = code)
  
TeamChosen = getTeam(TeamCodeChosen)

TeamNameChosen = TeamChosen$name
TeamPrimaryChosen = TeamChosen$primaryColor
TeamSecondaryChosen = TeamChosen$secondaryColor
TeamLogoChosen = TeamChosen$images.crest
  
GamesPlayed = getTeamGames(TeamCodeChosen) %>%
  filter(status == "result") %>% 
  mutate(TeamCodeChosen.win = ifelse((TeamCodeChosen == home.code) == (home.score > away.score), "Win", "Loss"),
         team.code.against = ifelse(TeamCodeChosen == home.code, away.code, home.code),
         team.home.away = ifelse((TeamCodeChosen == home.code), "Home", "Away"),
         game.date = as.Date(date)) %>% 
  distinct(round.round, code, team.code.against, team.home.away, TeamCodeChosen.win, game.date) %>% 
  left_join(TeamAll, by = "team.code.against")

TeamPeople = getTeamPeople(TeamCodeChosen) %>% 
  filter(typeName == "Player") %>%
  mutate(Player_ID = paste0("P", trimws(person.code)),
         images.headshot = images.headshot %>%
           ifelse(is.na(.), club.images.crest, .) %>%
           paste0(., "?width=150")) %>% 
  distinct(Player_ID, images.headshot)

PlayerStats = NULL
for (i in 1:nrow(GamesPlayed)) {
  PlayerStats = bind_rows(
    PlayerStats,
    getGameBoxScore(GamesPlayed$code[i]) %>% .[["PlayerStats"]] %>% filter(Team == TeamCodeChosen) %>% 
      mutate(round.round = GamesPlayed$round.round[i],
             Player_ID = trimws(Player_ID), .before = 1)
  )
}
PlayerStats = PlayerStats %>%
  left_join(GamesPlayed, by = "round.round") %>%
  left_join(TeamPeople, by = "Player_ID")

library(ggplot2)
library(ggtext)
library(showtext)
library(ggfun)
library(ggimage)
library(ggnewscale)
library(glue)
library(lubridate)

font_add_google("Lato", "Lato")
sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "www/fonts/fa-brands-400.ttf")
showtext_auto() 

offset = 5

PlayerStatsForPlot = PlayerStats %>%
  group_by(Player) %>%
  mutate(median.plusminus = median(Plusminus) %>% ifelse(n() > 6, ., -30),
         Plusminus = case_when(Plusminus > 0 ~ Plusminus + offset, 
                               Plusminus < 0 ~ Plusminus - offset, TRUE ~ Plusminus)) %>% 
  ungroup() %>%
  mutate(Player = paste0(gsub(".*, ", "", Player), " ", gsub(",.*", "", Player), " #", Dorsal)) %>% 
  arrange(desc(median.plusminus)) %>% 
  mutate(Player = factor(Player, levels = unique(.$Player))) %>% 
  arrange(round.round)

RangeX = range(PlayerStatsForPlot$round.round)
LastN = RangeX[2] - 1
XStart = RangeX[2] - LastN - 0.5
XEnd = RangeX[2] + 0.5

BreaksY = c(-(offset + seq(30, 0, -15)), (offset + seq(0, 30, 15)))
LabelsY = c(-(seq(30, 0, -15)), (seq(0, 30, 15)))

SegmentY = expand.grid(YStart = BreaksY,
                       Player = unique(PlayerStatsForPlot$Player)) %>% 
  as_tibble() %>%
  mutate(YEnd = YStart, XStart = XStart, XEnd = XEnd)

TeamHome = PlayerStatsForPlot %>% 
  distinct(Player, round.round, team.home.away, Plusminus) %>% 
  mutate(Plusminus = sign(Plusminus)*3 + Plusminus)

TeamAgainstImage = PlayerStatsForPlot %>% 
  distinct(Player, round.round, team.code.against, team.home.away, images.crest) %>% 
  mutate(Plusminus = 0)

PlayerImage = PlayerStatsForPlot %>% 
  distinct(Player, images.headshot) %>% 
  mutate(round.round = (RangeX[2]-LastN + RangeX[2])/2, Plusminus = -5)

GameRangeDate = range(PlayerStatsForPlot$game.date) %>% format(., "%d %b %Y")

PlotTitle = glue("<span>Plus-Minus</span><br>
                  <span style = 'font-size: 20px'>{TeamNameChosen}
                  ({GameRangeDate[1]} - {GameRangeDate[2]})</span>")

PlotSubtitle = glue("<span><img src = '{TeamLogoChosen}' height='50'></span>
                     <span><img src = 'www/images/E2023-logo-vertical-black.png' height='50'></span>")

PlotCaption = glue("<span> Visualization by Flavio Leccese</span><br>
                    <span style = 'font-family:\"Font Awesome 6 Brands\";'>&#xf09b;</span>
                    <span>flavioleccese92</span>
                    <span style = 'font-family:\"Font Awesome 6 Brands\";'>&#xf08c;</span>
                    <span>flavioleccese</span>")

PlayerStatsForPlot %>% 
  ggplot(aes(x = round.round, y = Plusminus, group = Player)) +
  geom_bar(aes(fill = TeamCodeChosen.win), stat = "identity", size = 0.75) +
  scale_fill_manual("Match Result", values = c("Win" = "#2EB086", "Loss" = "#C70D3A")) +
  new_scale("fill") +
  geom_point(aes(fill = team.home.away), shape = 21, alpha = 0.50, data = TeamHome) +
  scale_fill_manual("Match Location", values = c("Home" = "black", "Away" = "white")) +
  geom_rect(xmin = XStart, ymin = -offset, xmax = XEnd, ymax = offset, fill = "#eeeeee") +
  geom_segment(aes(x = XStart, xend = XEnd, y = YStart, yend = YEnd),
               data = SegmentY, colour = "grey", alpha = 0.50) +
  geom_image(aes(image = images.crest), data = TeamAgainstImage, size = 0.1) +
  geom_image(aes(image = images.headshot), data = PlayerImage,
             image_fun = function(img) {magick::image_fx(img, expression = "0.15*a", channel = "alpha")},
             size = 1) +
  facet_wrap(~Player, nrow = 4, scales='free') +
  scale_y_continuous(limits = c(-30 - offset, 30 + offset), 
                     breaks = BreaksY, labels = LabelsY) +
  scale_x_continuous(limits = c(RangeX[2] - LastN - 1.5, RangeX[2] + 0.5), 
                     breaks = seq(RangeX[2] - LastN, RangeX[2], by = 1)) +
  theme(axis.ticks = element_blank(),
        axis.title.y = element_text(margin = margin(l = 5, r = 10)),
        axis.text.y = element_text(margin = margin(r = -15)),
        axis.title.x = element_text(margin = margin(t = 10, b = 5)),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.box.spacing = unit(0, "pt"),
        plot.title = element_markdown(lineheight = 1, size = 20, hjust = 0, vjust = 1, margin = margin(0, 0, -20, 0)),
        plot.subtitle = element_markdown(hjust = 1, margin = margin(-30, 3, -50, 0)),
        plot.caption = element_markdown(size = 12, margin = margin(-35, 0, 0, 3)),
        plot.caption.position = "plot",
        text = element_text(family = "Lato"),
        strip.background = element_rect(fill = TeamPrimaryChosen),
        strip.text = element_text(colour = TextContrast(TeamPrimaryChosen)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.margin = margin(20, 5, -5, 5)) +
  guides(color = guide_legend(override.aes = list(fill = "NA"))) +
  labs(title = PlotTitle, subtitle = PlotSubtitle, caption = PlotCaption,
       x = "Round", y = "Plus-Minus")
