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

Sys.setlocale(locale = "en_EN.UTF-8")

#### Import Data ####

TeamAll = getTeam() %>% distinct(TeamCode, TeamName, ImagesCrest) %>% 
  mutate(ImagesCrest = paste0(ImagesCrest, "?width=250"))

#### Setting ggplot2 ####

# Add Lato font (Euroleague official font)
font_add_google("Lato", "Lato")
font_add_google("Inconsolata", "Inconsolata")

# Add Font Awesome for logos
sysfonts::font_add(family = "Font Awesome 6 Brands", regular = "www/fonts/fa-brands-400.ttf")
showtext_opts(dpi = 200)
showtext_auto()
 
stats = c("PTS", "PIR", "2FGM", "2FG%", "3FGM", "3FG%", "FT%", "AST", "OREB", "DREB", "STL", "BLK", "TO") %>% rev()
gstats = stats %>% paste0("G",. )

TeamAllStats = getTeamAllStats(TeamAll$TeamCode)

TeamStatsForPlotBar = TeamAllStats %>%
  select(TeamCode, HomeAway, WinLoss, starts_with("G"), -GameCode) %>% 
  distinct() %>% 
  mutate(across(-c("GP", "TeamCode", "HomeAway", "WinLoss"), ~.*GP)) %>%
  group_by(TeamCode, HomeAway) %>%
  summarise(across(-c("WinLoss"), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
  mutate(`GFG%` = 100*(`G2FGM` + `G3FGM`)/(`G2FGA` + `G3FGA`),
         `G2FG%` = 100*`G2FGM`/`G2FGA`,
         `G3FG%` = 100*`G3FGM`/`G3FGA`,
         `GFT%` = 100*`GFTM`/`GFTA`) %>% 
  ungroup() %>% 
  mutate(across(-c("GP", ends_with("%")) & starts_with("G"), ~round(./GP, 2)),
         across(ends_with("%"), ~round(., 2)),
         across(everything(), ~ifelse(is.nan(.), NA, .))) %>% 
  select(TeamCode, HomeAway, all_of(gstats)) %>% 
  distinct() %>% 
  mutate(across(all_of(gstats), ~ ntile(., 5))) %>%
  pivot_longer(cols = all_of(gstats), names_to = "Stat", values_to = "Value") %>% 
  mutate(ActualValue = Value) %>% 
  group_by(TeamCode, HomeAway, Stat) %>%
  complete(Value = 0:5)  %>%
  mutate(ActualValue = max(ActualValue, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(AlphaValue = case_when(Value <= 0 ~ "Empty", Value == ActualValue ~ "Actual",
                                Value < ActualValue ~ "Less", TRUE ~ "Empty"),
         FillValue = case_when(Value == 5 ~ "High", Value == 4 ~ "Mid - High", 
                               Value == 3 ~ "Medium", Value == 2 ~ "Mid - Low", 
                               Value == 1 ~ "Low", TRUE ~ NA) %>% 
           factor(., levels = c("High", "Mid - High", "Medium", "Mid - Low", "Low")),
         GroupValue = factor(Value, levels = 5:0),
         SizeValue = ifelse(Value <= 0, "Empty", "Colored")
  )  %>%
  ungroup() %>% 
  left_join(TeamAll, by = "TeamCode") %>%
  left_join(CompetitionStanding %>% select(TeamCode = ClubCode, Position), by = "TeamCode") %>% 
  arrange(Position) %>% 
  mutate(Team = glue("{TeamName} #{Position}")) %>% 
  mutate(Team = factor(Team, levels = unique(.$Team)),
         Stat = factor(Stat, levels = gstats))

TeamStatsForPlotPoint = TeamAllStats %>%
  select(TeamCode, WinLoss, HomeAway, all_of(gstats)) %>% 
  distinct() %>% 
  mutate(across(all_of(gstats), ~ ntile(., 20)),
         across(all_of(gstats), ~ rescale(./max(.), to = c(1/6, 1)))) %>%
  pivot_longer(cols = all_of(gstats), names_to = "Stat", values_to = "Value") %>% 
  left_join(TeamAll, by = "TeamCode") %>%
  left_join(CompetitionStanding %>% select(TeamCode = ClubCode, Position), by = "TeamCode") %>% 
  mutate(Team = glue("{TeamName} #{Position}")) %>% 
  mutate(Team = factor(Team, levels = levels(TeamStatsForPlot$Team)),
         Stat = factor(Stat, levels = gstats),
         WinLoss = factor(WinLoss, levels = c("Win", "Loss"))) %>% 
  distinct()

TeamHomeAwayLabels = TeamAllStats %>% 
  left_join(TeamAll, by = "TeamCode") %>%
  left_join(CompetitionStanding %>% select(TeamCode = ClubCode, Position), by = "TeamCode") %>% 
  mutate(Team = glue("{TeamName} #{Position}")) %>%
  distinct(Team, HomeAway, WinLoss, GP) %>% 
  pivot_wider(values_from = GP, names_from = WinLoss, values_fill = 0) %>% 
  mutate(Team = factor(Team, levels = levels(TeamStatsForPlot$Team)),
         Label = ifelse(HomeAway == "Home", glue("Home ({Win}W / {Loss}L)"),
                        glue("({Win}W / {Loss}L) Away")),
         y = "Image",
         x = ifelse(HomeAway == "Home", -1, 1)) %>% 
  distinct(Team, x, y, HomeAway, Label)

TeamImage = TeamStatsForPlot %>% 
  filter(!is.na(TeamLogo)) %>% 
  distinct(Team, TeamLogo) %>% 
  mutate(Stat = "Image", x = 0)

YLabels = tibble(YLabel = stats, y = gstats)
  
# Plot title, subtitle and caption
PlotTitle = glue("<span>Team combined statistics by location and result</span><br>
                  <span style = 'font-size: 20px'>All teams | Up to round {(max(CompetitionRounds$Round))} | 
                 {format(as.Date(max(CompetitionRounds$MaxGameStartDate)), '%d %b %Y')}</span>")

PlotSubtitle = glue(" <span><img src = 'www/images/E2023-logo-vertical-black.png' height='50'></span>")

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
e = ggplot(TeamImage, aes(y = Stat))

# Draw background rectangles and Team image
e = e + 
  geom_image(data = TeamImage, aes(x = x, image = TeamLogo), size = 0.15,
             image_fun = function(img) { magick::image_crop(img) })

# Draw bars of stats
e = e +
  geom_bar(data = TeamStatsForPlotBar %>% filter(HomeAway == "Home"),
           aes(x = -1, fill = FillValue, group = GroupValue, 
               alpha = AlphaValue, linewidth = SizeValue),
           position = "fill", stat = "identity", colour = "#eeede9")

e = e +
  geom_bar(data = TeamStatsForPlotBar %>% filter(HomeAway == "Away"),
           aes(x = 1, fill = FillValue, group = GroupValue, 
               alpha = AlphaValue, linewidth = SizeValue),
           position = "fill", stat = "identity", colour = "#eeede9") + 
  scale_alpha_manual(values = c("Actual" = 1, "Less" = 0.25, "Empty" = 0), guide = "none") +
  scale_linewidth_manual(values = c("Empty" = 0, "Colored" = 0.5), guide = "none") +
  scale_fill_manual(name = "Ranking",
                    values = c("Low" = "#C70D3A", "Mid - Low" = "#FF7F00", "Medium" = "#FFD301", 
                               "Mid - High" = "#7BB662", "High" = "#2EB086"))

# Add points W/L
e = e +
  new_scale("fill")  +
  scale_fill_manual("Match result", values = c("Win" = "#2EB086", "Loss" = "#C70D3A")) +
  geom_point(data = TeamStatsForPlotPoint %>% filter(HomeAway == "Away"),
             aes(x = Value, y = Stat, fill = WinLoss), 
             shape = 21, alpha = 1, colour = "#eeede9", size = 2.3)

e = e +
  geom_point(data = TeamStatsForPlotPoint %>% filter(HomeAway == "Home"),
             aes(x = -Value, y = Stat, fill = WinLoss), 
             shape = 21, alpha = 1, colour = "#eeede9", size = 2.3)

# Add Text
e = e +
  geom_text(data = YLabels, aes(y = y, label = YLabel), colour = "#404040", x = 0,
            size = 2.5) +
  geom_text(data = TeamHomeAwayLabels %>% filter(HomeAway == "Home"), aes(y = y, x = x, label = Label),
            colour = "#404040", size = 3, hjust = 0) +
  geom_text(data = TeamHomeAwayLabels %>% filter(HomeAway == "Away"), aes(y = y, x = x, label = Label),
            colour = "#404040", size = 3, hjust = 1)

# Facet by Team + general theme setting
e = e +
  facet_wrap(~ Team, ncol = 6) +
  scale_y_discrete(limits = c("", gstats, "", "Image", ""))  +
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
    axis.text.x = element_blank(),
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
    strip.background = element_rect(fill = "#F47321"),
    strip.text = element_text(colour = "black", hjust = 0)
  )  +
  labs(title = PlotTitle, subtitle = PlotSubtitle, caption = PlotCaption,
       x = "", y = "")

agg_png(glue("plots/E2023/ALL TEAMS/team_stats_quant_ha_wl.png"),
        height = 2000, width = 4100, units = "px", res = 200, background = "transparent")
print(e)
dev.off()

