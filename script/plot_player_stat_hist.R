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

TeamAll = getTeam() %>% distinct(TeamCode, ImagesCrest) %>% 
  mutate(ImagesCrest = paste0(ImagesCrest, "?width=250"))

#### Setting ggplot2 ####

# Add Lato font (Euroleague official font)
font_add_google("Lato", "Lato")
font_add_google("Inconsolata", "Inconsolata")

# Add Font Awesome for logos
sysfonts::font_add(family = "Font Awesome 6 Brands", regular = "www/fonts/fa-brands-400.ttf")
showtext_opts(dpi = 200)
showtext_auto() 

for (team_code in TeamAll$TeamCode) {
  
  TeamChosen = getTeam(team_code)
  
  TeamNameChosen = TeamChosen$TeamName %>% print()
  TeamPrimaryChosen = TeamChosen$PrimaryColor
  TeamSecondaryChosen = TeamChosen$SecondaryColor
  TeamLogoChosen = glue("_temp/{team_code}/{team_code}-logo.png")
  
  TeamPeople = getTeamPeople(team_code) %>% 
    filter(TypeName == "Player") %>%
    mutate(Player_ID = trimws(PersonCode),
           ImagesHeadshot = ifelse(is.na(ImagesHeadshot),
                                   "www/images/missing-player.png",
                                   glue("_temp/{team_code}/{Player_ID}.png")),
           ActivePlayer = as.Date(EndDate) >= Sys.Date()) %>% 
    distinct(Player_ID, ImagesHeadshot, ActivePlayer)
  
  PlayerStats = GetPlayerAllStats(team_code) %>%
    left_join(TeamPeople, by = "Player_ID") %>%
    left_join(TeamAll, by = c("TeamCodeAgainst" = "TeamCode")) %>%
    filter(ActivePlayer) %>%
    mutate(Player = paste0(gsub(".*, ", "", Player), " ", gsub(",.*", "", Player), " #", Dorsal))
  
  #### Setup plot ####
  for (stat in c("PM", "FG%", "3FG%", "2FG%", "FTG%", "PTS", "PIR")) {
  # stat = "3FG%"; 
  print(stat)
  gstat = glue("G{stat}")
  StatsRangeForPlot = StatsRange %>% filter(Stat == stat)
  
  # Vertical
  YStart = StatsRangeForPlot$Min; YEnd = StatsRangeForPlot$Max; YBy = StatsRangeForPlot$By
  BottomMargin = StatsRangeForPlot$BottomMargin; TopMargin = StatsRangeForPlot$TopMargin
  BottomOffset = StatsRangeForPlot$BottomOffset; MiddleOffset = StatsRangeForPlot$MiddleOffset
  Unit = StatsRangeForPlot$Unit
  if (MiddleOffset == 0) {
    BottomMargin = BottomMargin/100*(YEnd - YStart)
    BottomOffset = BottomOffset/100*(YEnd - YStart)
    TopMargin = TopMargin/100*(YEnd - YStart)
    YUpperZero = BottomMargin + BottomOffset; YLowerZero = 0
    YUpperLimit = YEnd + BottomMargin + BottomOffset + TopMargin; YLowerLimit = YStart
    BreaksY = BottomMargin + BottomOffset + seq(0, YEnd, YBy)
    LabelsY = seq(0, YEnd, YBy); ShiftStat = BottomMargin + BottomOffset
  } else {
    YUpperZero = MiddleOffset/2; YLowerZero = - MiddleOffset/2
    YUpperLimit = YEnd + TopMargin + MiddleOffset/2; YLowerLimit = YStart - BottomMargin - MiddleOffset/2
    BreaksY = c((-MiddleOffset/2 + seq(YStart, 0, YBy)), (MiddleOffset/2 + seq(0, YEnd, YBy)))
    LabelsY = c(seq(YStart, 0, YBy), seq(0, YEnd, YBy)); ShiftStat = MiddleOffset/2
  }
  YSpan = YUpperLimit - YLowerLimit
  YCenter = (YUpperLimit + YLowerLimit)/2
  
  OffsetX = 5 # Left shift to make space for player image
  
  LastN = 16 # Last N rounds to plot
  
  RangeX = range(PlayerStats$Round) # Range of rounds for chosen team, season and competition
  XStart = RangeX[2] - LastN + 1 # Starting x for actual data plot
  XEnd = RangeX[2] # Ending x for actual data plot
  XLowerLimit = RangeX[2] - LastN - OffsetX # Lower limit for x axis (player image + data)
  XUpperLimit = RangeX[2] + 2 # Upper limit for x axis (data + y axis annotation)
  GameRangeDate = format(sort(unique(PlayerStats$GameDate)), "%d %b %Y")[c(XStart, XEnd)] # Format date of games
  
  PlayerStatsForPlot = PlayerStats %>%
    select(everything(),
           stat = {{stat}}, gstat = {{gstat}}) %>%
    mutate(stat = case_when(stat > 0 ~ stat + ShiftStat, 
                            stat < 0 ~ stat - ShiftStat,
                            TRUE ~ ShiftStat),
           gstat = gstat %>% ifelse(GP > 6, ., NA)) %>% 
    ungroup() %>%
    filter(Round > XEnd - LastN)%>% 
    arrange(desc(gstat)) %>% 
    mutate(gstat = gstat %>% ifelse(is.na(.), "-", .),
           Player = glue("{Player} (avg {gstat}{Unit} / {GP} games)"))  %>% 
    filter(Player %in% unique(.$Player)[1:16]) %>% 
    mutate(Player = factor(Player, levels = unique(.$Player))) %>% 
    arrange(Round) %>% 
    distinct(Round, GameDate, Player, stat,
             HomeAway, WinLoss, ImagesHeadshot, ImagesCrest)
  
  ZPlayers = unique(PlayerStatsForPlot$Player) # Facet unique players
  
  # Data for horizontal lines (y values) and y axis annotation
  SegmentY = expand.grid(YStart = BreaksY,
                         Player = ZPlayers) %>% 
    as_tibble() %>%
    mutate(YEnd = YStart, XStart = XStart - 1.5, XEnd = XEnd,
           YLabel = glue("{YStart - sign(YStart)*ShiftStat}{Unit}"))
  
  # Data for points above/below bars (+/- 3) corresponding to Home/Away Location
  TeamHome = PlayerStatsForPlot %>% 
    distinct(Player, Round, HomeAway, stat) %>% 
    mutate(stat = sign(stat)*YSpan/30 + stat)
  
  # Data for Team Against images, showing in the middle of plot
  TeamAgainstImage = PlayerStatsForPlot %>% 
    distinct(Player, Round, ImagesCrest) %>% 
    # mutate(stat = BottomMargin)
    mutate(stat = max(BottomOffset, 0))
  
  # Data for Player image, showing on the left side (XLowerLimit + 3)
  PlayerImage = PlayerStatsForPlot %>% 
    distinct(Player, ImagesHeadshot) %>% 
    mutate(Round = XLowerLimit + 2.5, stat = YCenter)
  
  # Plot title, subtitle and caption
  PlotTitle = glue("<span>{stat}</span><br>
                    <span style = 'font-size: 20px'>{team_code}
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
             aes(x = Round, y = stat, group = Player))
  
  # Draw horizontal lines + y labels + plot polygon left (aesthetic choice) 
  e = e +
    geom_link(data = SegmentY, aes(x = XStart, xend = XEnd + 0.5, y = YStart, yend = YEnd,
                                   colour = after_stat(index)), linewidth = 0.2) +
    geom_text(data = SegmentY, aes(y = YStart, label = YLabel), colour = "#404040", x = XEnd + 1.8,
              size = 2.5, hjust = 1) +
    scale_colour_gradient(low = TeamPrimaryChosen, high = "#404040") +
    annotate(geom = "polygon", fill = "#eeede9",
             x = c(XLowerLimit, XLowerLimit, XStart - 1.5, XStart), 
             y = c(YUpperLimit, YLowerLimit, YLowerLimit, YUpperLimit))
  
  # Draw bars for statistics + scale fill values + cover central part of bars with background
  e = e + 
    geom_bar(aes(fill = WinLoss), stat = "identity", linewidth = 0.75, alpha = 0.8) +
    scale_fill_manual("Match Result", values = c("Win" = "#2EB086", "Loss" = "#C70D3A")) +
    geom_rect(xmin = XStart - 0.5, xmax = XEnd + 0.5,
              ymin = YLowerZero - sign(YLowerZero)*0.4, ymax = YUpperZero - 0.5, fill = "#e2e7ea")
  
  # Draw points for Home/Away location + new scale fill values
  e = e +
    new_scale("fill") +
    geom_point(data = TeamHome, aes(fill = HomeAway), shape = 21, alpha = 0.50) +
    scale_fill_manual("Match Location", values = c("Home" = "#404040", "Away" = "#eeede9"))
  
  # Plot player image + team against images in the middle
  e = e +
    geom_image(data = TeamAgainstImage, aes(image = ImagesCrest), size = 0.1) +
    geom_image(data = PlayerImage, aes(image = ImagesHeadshot), size = 0.8,
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
      panel.background = element_rect_round(fill = "#e2e7ea"),
      plot.background = element_rect(fill = "#eeede9"),
      plot.margin = margin(32, 15, 8, 17),
      text = element_text(color = "#404040", family = "Lato"),
      # Axis labels
      axis.ticks = element_blank(),
      # axis.title.y = element_text(margin = margin(l = 5, r = 10)),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      # axis.title.x = element_text(margin = margin(t = 10, b = 5)),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      # Legend,
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
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
  agg_png(glue("plots/E2023/{team_code}/player_{tolower(gsub('%', '_perc', stat))}.png"),
          height = 2000, width = 4100, units = "px", res = 200)
  print(e)
  dev.off()
  }
}
