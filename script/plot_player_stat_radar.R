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
library(geomtextpath)
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

TeamAllStats = getTeamStats(TeamAll$TeamCode)

stats = c("PTS", "2FGM", "2FG%", "3FGM", "3FG%", "FT%", "AST", "REB", "STL", "BLK", "TO")

TeamPeople = getTeamPeople(TeamAll$TeamCode) %>% 
  filter(TypeName == "Player") %>%
  mutate(Player_ID = PersonCode,
         ImagesHeadshot = ifelse(is.na(ImagesHeadshot),
                                 "www/images/missing-player.png",
                                 glue("_temp/{TeamCode}/{Player_ID}.png")),
         ActivePlayer = as.Date(EndDate) >= Sys.Date()) %>% 
  distinct(TeamCode, Player_ID, Player, PositionName, ImagesHeadshot, ActivePlayer)

for (team_code in TeamAll$TeamCode) {
  
  TeamChosen = getTeam(team_code)
  
  TeamNameChosen = TeamChosen$TeamName %>% print()
  TeamPrimaryChosen = TeamChosen$PrimaryColor
  TeamSecondaryChosen = TeamChosen$SecondaryColor
  TeamLogoChosen = glue("_temp/{team_code}/{team_code}-logo.png")
  
  for (type in c("All competition players", "Competition players by role")) {
  print(type)
  
  PlayerAveragePerGameQuantile = TeamAllStats$PlayerAveragePerGame %>% 
    left_join(TeamPeople, by = c("TeamCode", "Player_ID")) %>%
    filter(ActivePlayer) %>% 
    {if (type == "Competition players by role")
      select(., TeamCode, Player, GP, PositionName, ImagesHeadshot, all_of(stats)) %>%
        mutate(Player = glue("{Player} ({PositionName})")) %>% 
        group_by(PositionName) 
      else 
        select(., TeamCode, Player, GP, ImagesHeadshot, all_of(stats)) } %>% 
    mutate(., across(all_of(stats), ~ ntile(., 5))) %>%
    ungroup() %>% 
    pivot_longer(cols = all_of(stats), names_to = "Stat", values_to = "Value")
    
    PlayerStatsForPlot = PlayerAveragePerGameQuantile %>%
      filter(TeamCode == team_code) %>% 
      mutate(ActualValue = Value) %>% 
      group_by(TeamCode, Player, Stat) %>% 
      complete(Value = -3:5) %>% 
      mutate(ActualValue = max(ActualValue, na.rm = TRUE)) %>% 
      ungroup() %>%
      mutate(AlphaValue = case_when(Value <= 0 ~ "Empty", Value == ActualValue ~ "Actual",
                                    Value < ActualValue ~ "Less", TRUE ~ "Empty"),
             FillValue = case_when(Value == 5 ~ "High", Value == 4 ~ "Mid - High", 
                                   Value == 3 ~ "Medium", Value == 2 ~ "Mid - Low", 
                                   Value == 1 ~ "Low", TRUE ~ NA) %>% 
               factor(., levels = c("High", "Mid - High", "Medium", "Mid - Low", "Low")),
             GroupValue = factor(Value, levels = 5:-3),
             SizeValue = ifelse(Value <= 0, "Empty", "Colored")
             ) %>%
      arrange(desc(GP)) %>% 
      mutate(Player = factor(Player, levels = unique(.$Player)))
    
    PlayerImage = PlayerStatsForPlot %>% 
      filter(!is.na(ImagesHeadshot)) %>% 
      distinct(Player, ImagesHeadshot) %>% 
      mutate(Stat = 4, y = 0)
    
    # Plot title, subtitle and caption
    PlotTitle = glue("<span>Combined Statistics</span><br>
                      <span style = 'font-size: 20px'>{TeamNameChosen} | All rounds | {type}</span>")
    
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
    e = ggplot(PlayerStatsForPlot, aes(x = Stat))
    
    # Draw background rectangles and player image
    e = e +
      geom_rect(xmin = -Inf, xmax = +Inf, ymin = 0, ymax = 0.4, fill = "#eeede9") + 
      geom_image(data = PlayerImage, aes(y = y, image = ImagesHeadshot), size = 0.25,
                 image_fun = function(img) {
                   magick::image_crop(img, "700x700+0-380")}) +
      geom_rect(xmin = -Inf, xmax = +Inf, ymin = 0.4, ymax = 1, fill = "#e2e7ea")
    
    # Draw bars of stats
    e = e +
      geom_bar(aes(y = 1, fill = FillValue, group = GroupValue, 
                   alpha = AlphaValue, linewidth = SizeValue),
               position = "fill", stat = "identity", colour = "#eeede9")
    
    
    # Facet by Player + general theme setting
    e = e +
      facet_wrap(~ Player, ncol = 6) +
      scale_x_discrete() + 
      coord_curvedpolar(clip = "off") +
      scale_alpha_manual(values = c("Actual" = 1, "Less" = 0.25, "Empty" = 0), guide = "none") +
      scale_linewidth_manual(values = c("Empty" = 0, "Colored" = 0.5), guide = "none") +
      scale_fill_manual(name = "Ranking",
                        values = c("Low" = "#C70D3A", "Mid - Low" = "#FF7F00", "Medium" = "#FFD301", 
                                   "Mid - High" = "#7BB662", "High" = "#2EB086")) +
      scale_hjust_manual(values = 0.8) +
      theme(
        # General
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
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
        axis.text.x = element_text(vjust = 0.5),
        # Legend
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
      )  +
      labs(title = PlotTitle, subtitle = PlotSubtitle, caption = PlotCaption,
           x = "", y = "")
    
    suffix = ifelse (grepl("role", type), "_role", "")
    agg_png(glue("plots/E2023/{team_code}/player_stats_quant{suffix}.png"),
            height = 2000, width = 4100, units = "px", res = 200, background = "transparent")
    print(e)
    dev.off()
  }
}

