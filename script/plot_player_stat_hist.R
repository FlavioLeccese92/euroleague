### ------------------------------------------------------------------------ ###
####------------------------------ PLUS-MINUS ------------------------------####
### ------------------------------------------------------------------------ ###

library(dplyr)
library(ggplot2)
library(ggtext)
library(showtext)
library(ggfun)
library(ggimage)
library(ggnewscale)
library(ggforce)
library(glue)
library(ragg)
library(elementalist) # devtools::install_github("teunbrand/elementalist")
# library(magick) 

Sys.setlocale(locale = "es_ES.UTF-8")

#### Import Data ####

TeamAll = getTeam() %>% distinct(code, images.crest) %>% 
  mutate(images.crest = paste0(images.crest, "?width=250")) %>%
  rename(team.code.against = code)

#### Setting ggplot2 ####

# Add Lato font (Euroleague official font)
font_add_google("Lato", "Lato")
font_add_google("Inconsolata", "Inconsolata")

# Add Font Awesome for logos
sysfonts::font_add(family = "Font Awesome 6 Brands", regular = "www/fonts/fa-brands-400.ttf")
showtext_opts(dpi = 200)
showtext_auto() 

for (TeamCodeChosen in TeamAll$team.code.against) {
  
  TeamChosen = getTeam(TeamCodeChosen)
  
  TeamNameChosen = TeamChosen$name %>% print()
  TeamPrimaryChosen = TeamChosen$primaryColor
  TeamSecondaryChosen = TeamChosen$secondaryColor
  TeamLogoChosen = glue("_temp/{TeamCodeChosen}/{TeamCodeChosen}-logo.png")
  
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
    mutate(Player_ID = trimws(person.code),
           images.headshot = ifelse(is.na(images.headshot),
                                    "www/images/missing-player.png",
                                    glue("_temp/{TeamCodeChosen}/{Player_ID}.png")),
           active.player = as.Date(endDate) >= Sys.Date()) %>% 
    distinct(Player_ID, images.headshot, active.player)
  
  PlayerStats = NULL
  for (i in 1:nrow(GamesPlayed)) {
    PlayerStats = bind_rows(
      PlayerStats,
      getGameBoxScore(GamesPlayed$code[i]) %>% .[["PlayerStats"]] %>% filter(Team == TeamCodeChosen) %>% 
        mutate(round.round = GamesPlayed$round.round[i],
               Player_ID = trimws(gsub("P", "", Player_ID)), .before = 1)
    )
  }
  PlayerStats = PlayerStats %>%
    left_join(GamesPlayed, by = "round.round") %>%
    left_join(TeamPeople, by = "Player_ID") %>%
    filter(active.player) %>%
    mutate(Player = paste0(gsub(".*, ", "", Player), " ", gsub(",.*", "", Player), " #", Dorsal))
  
  #### Setup plot ####
  
  stat = "Valuation"
  YEnd = case_when(stat == "Points" ~ 40,
                   stat ==  "Valuation" ~ 45,
                   TRUE ~ NA)
  
  OffsetY = 0.2*YEnd # Bottom and Upper shift to make space for team images
  OffsetX = 5 # Left shift to make space for player image
  
  LastN = 16 # Last N rounds to plot
  
  RangeX = range(PlayerStats$round.round) # Range of rounds for chosen team, season and competition
  XStart = RangeX[2] - LastN + 1 # Starting x for actual data plot
  XEnd = RangeX[2] # Ending x for actual data plot
  XLowerLimit = RangeX[2] - LastN - OffsetX # Lower limit for x axis (player image + data)
  XUpperLimit = RangeX[2] + 2 # Upper limit for x axis (data + y axis annotation)
  YLowerLimit = 0 # Lower limit for y axis (data + middle space for teams logo + spacing)
  YUpperLimit = YEnd + 2*OffsetY # Upper limit for y axis (data + middle space for teams logo + spacing)
  GameRangeDate = format(sort(unique(PlayerStats$game.date)), "%d %b %Y")[c(XStart, XEnd)] # Format date of games
  
  BreaksY = OffsetY + seq(0, YEnd, 10)
  LabelsY = seq(0, YEnd, 10)
  
  PlayerStatsForPlot = PlayerStats %>% 
    group_by(Player) %>%
    filter(Minutes  != "DNP") %>%
    mutate(n.games = n(),
           mean.stats = mean(!!sym(stat)) %>% round(., 2),
           stat = !!as.symbol(stat) + OffsetY,
           Player = paste0(Player, " (avg ", mean.stats, " / ", n.games, " games)"),
           mean.stats = mean.stats %>% ifelse(n.games > 6, ., -30)) %>% 
    ungroup() %>%
    filter(round.round > XEnd - LastN) %>% 
    arrange(desc(mean.stats)) %>% 
    filter(Player %in% unique(.$Player)[1:16]) %>%
    mutate(Player = factor(Player, levels = unique(.$Player))) %>% 
    arrange(round.round) %>% 
    distinct(round.round, game.date, Player, stat,
             team.home.away, TeamCodeChosen.win, images.headshot, images.crest)
  
  ZPlayers = unique(PlayerStatsForPlot$Player) # Facet unique players
  
  # Data for horizontal lines (y values) and y axis annotation
  SegmentY = expand.grid(YStart = BreaksY,
                         Player = ZPlayers) %>% 
    as_tibble() %>%
    mutate(YEnd = YStart, XStart = XStart - OffsetX - 1, XEnd = XEnd,
           YLabel = YStart - OffsetY)
  
  # Data for points above bars (+3) corresponding to Home/Away Location
  TeamHome = PlayerStatsForPlot %>% 
    distinct(Player, round.round, team.home.away, stat) %>% 
    mutate(stat = 3 + stat)
  
  # Data for Team Against images, showing in the middle of plot
  TeamAgainstImage = PlayerStatsForPlot %>% 
    distinct(Player, round.round, images.crest) %>% 
    mutate(stat = OffsetY/2)
  
  # Data for Player image, showing on the left side (XLowerLimit + 2)
  PlayerImage = PlayerStatsForPlot %>% 
    distinct(Player, images.headshot) %>% 
    mutate(round.round = XLowerLimit + 3, stat = (YUpperLimit+YLowerLimit)/2*0.9)
  
  # Plot title, subtitle and caption
  PlotTitle = glue("<span>{stat}</span><br>
                    <span style = 'font-size: 20px'>{TeamNameChosen}
                    | round {XStart} - {XEnd} | {GameRangeDate[1]} - {GameRangeDate[2]}</span>")
  
  PlotSubtitle = glue("<span><img src = '{TeamLogoChosen}' height='50'></span>
                       <span><img src = 'www/images/E2023-logo-vertical-black.png' height='50'></span>")
  
  PlotCaption = glue("<span>Visualization with </span>
                      <span style = 'font-family:\"Inconsolata\";'>R</span>
                      <span>and</span>
                      <span style = 'font-family:\"Inconsolata\";'>ggplot2</span>
                      <span>by Flavio Leccese |</span>
                      <span style = 'font-family:\"Font Awesome 6 Brands\";'>&#xf09b;</span>
                      <span>flavioleccese92</span>
                      <span style = 'font-family:\"Font Awesome 6 Brands\";'>&#xf08c;</span>
                      <span>flavioleccese</span>")
  
  ### Generate plot ###
  
  # Initialize
  e = ggplot(data = PlayerStatsForPlot, 
             aes(x = round.round, y = stat, group = Player))
  
  # Draw horizontal lines + y labels + plot polygon left (aesthetic choice) 
  e = e +
    geom_link(data = SegmentY, aes(x = XStart, xend = XEnd + 0.5, y = YStart, yend = YEnd,
                                   colour = after_stat(index)), linewidth = 0.3) +
    geom_text(data = SegmentY, aes(y = YStart, label = YLabel), colour = TeamPrimaryChosen, x = XEnd + 1,
              size = 2.5) +
    scale_colour_gradient2(low =  "white", mid = "white", high = TeamPrimaryChosen) +
    annotate(geom = "polygon", fill = "white",
             x = c(XLowerLimit, XLowerLimit, XStart - 1.5, XStart), 
             y = c(YUpperLimit, YLowerLimit, YLowerLimit, YUpperLimit))
  
  # Draw bars for statistics + scale fill values + cover central part of bars with background
  e = e + 
    geom_bar(aes(fill = TeamCodeChosen.win), stat = "identity", linewidth = 0.75, alpha = 0.8) +
    scale_fill_manual("Match Result", values = c("Win" = "#2EB086", "Loss" = "#C70D3A")) +
    geom_rect(xmin = XStart - 0.5, xmax = XEnd + 0.5,
              ymin = 0, ymax = OffsetY - 0.5, fill = "grey90")
  
  # Draw points for Home/Away location + new scale fill values
  e = e +
    new_scale("fill") +
    geom_point(data = TeamHome, aes(fill = team.home.away), shape = 21, alpha = 0.50) +
    scale_fill_manual("Match Location", values = c("Home" = "black", "Away" = "white"))
  
  # Plot player image + team against images in the middle
  e = e +
    geom_image(data = TeamAgainstImage, aes(image = images.crest), size = 0.1) +
    geom_image(data = PlayerImage, aes(image = images.headshot), size = 0.7,
               image_fun = function(img) {magick::image_fx(img, expression = "1*a", channel = "alpha")})
  
  # Facet by Player + general theme setting
  e = e +
    facet_wrap(~Player, ncol = 4, scales = 'free') +
    scale_y_continuous(limits = c(YLowerLimit, YUpperLimit), 
                       breaks = BreaksY, labels = LabelsY, expand = c(0, 0)) +
    scale_x_continuous(limits = c(XLowerLimit, XUpperLimit), expand = c(0, 0)) +
    theme(
      # General
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_rect_round(fill = "grey90"),
      plot.margin = margin(20, 5, -5, 5),
      text = element_text(family = "Lato"),
      # Axis labels
      axis.ticks = element_blank(),
      # axis.title.y = element_text(margin = margin(l = 5, r = 10)),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      # axis.title.x = element_text(margin = margin(t = 10, b = 5)),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      # Legend
      legend.position = 'bottom', 
      legend.justification = 'left',
      legend.direction = 'horizontal',
      legend.margin = margin(10, 0, 3, 0),
      legend.box.spacing = unit(0, "pt"),
      # Title, subtitle, caption
      plot.title = element_markdown(lineheight = 1, size = 20, hjust = 0, vjust = 1, margin = margin(0, 0, -20, 0)),
      plot.subtitle = element_markdown(hjust = 1, margin = margin(-30, 3, -50, 0)),
      plot.caption = element_markdown(size = 12, margin = margin(-25, 0, 0, 3)),
      plot.caption.position = "plot",
      # Facet
      strip.background = element_rect(fill = TeamPrimaryChosen),
      strip.text = element_text(colour = TextContrast(TeamPrimaryChosen), hjust = 0)
    ) +
    labs(title = PlotTitle, subtitle = PlotSubtitle, caption = PlotCaption,
         x = "", y = "")
  
  # Remove legend default background for fill + allow overflow of images
  e = e +
    guides(color = "none") +
    coord_cartesian(clip = "off")
  
  #### Save plot ####
  agg_png(glue("plots/E2023/{TeamCodeChosen}/player_{tolower(stat)}.png"),
          height = 2000, width = 4000, units = "px", res = 200)
  print(e)
  dev.off()
}
