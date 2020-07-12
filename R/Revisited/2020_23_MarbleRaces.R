## packages
library(tidyverse)
library(ggbump)
library(showtext)
library(pdftools)
library(ggplot2)
library(sysfonts)

#pongo esto para que pueda imprimirse en RStudio
trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)

font_add_google("Oswald", "Oswald")
showtext_auto()

## ggplot theme
theme_set(theme_void(base_family = "Oswald"))

theme_update(
  axis.text.y = element_text(
    size = 8, 
    color = "grey95", 
    margin = margin(r = 5)
  ),
  axis.text.x.top = element_text(
    size = 12,
    color = "grey95"
  ),
  axis.text.x.bottom = element_text(
    color = "black", 
    size = 1
  ),
  axis.title.x.top = element_text(
    color = "black", 
    size = 1
  ),
  axis.title.x.bottom = element_text(
    color = "grey95", 
    size = 18, 
    face = "bold"
  ),
  plot.margin = margin(15, 15, 5, 15),
  plot.background = element_rect(
    fill = "black", 
    color = "black"
  ),
  plot.caption = element_text(
    color = "grey30", 
    size = 10, 
    hjust = 1, 
    margin = margin(t = 25, b = 0)
  ),
  plot.caption.position = "plot"
)

df_marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

df_marbles_rank <-
  df_marbles %>% 
  dplyr::select(race, marble_name, team_name, points, time_s) %>% 
  filter(str_detect(race, "R")) %>% 
  group_by(race, team_name) %>% 
  summarize(points = unique(points)) %>% 
  group_by(team_name) %>% 
  arrange(team_name, race) %>% 
  mutate(
    points_sum = sum(points),
    points_cum = cumsum(points)
  ) %>%
  group_by(race) %>% 
  arrange(-points_cum, points_sum) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  mutate(
    race_num = as.numeric(str_remove(race, "S1R")),
    team_name = fct_reorder(team_name, -points_sum),
    team_abbr = case_when(
      team_name == "Savage Speeders" ~ "SAV",
      team_name == "Hazers" ~ "HAZ",
      team_name == "O'rangers" ~ "ORA",
      team_name == "Snowballs" ~ "SNO",
      team_name == "Green Ducks" ~ "GDK",
      team_name == "Team Galactic" ~ "TGL",
      team_name == "Team Primary" ~ "TPR",
      team_name == "Team Momo" ~ "TMO",
      team_name == "Thunderbolts" ~ "TDB",
      team_name == "Balls of Chaos" ~ "BOC",
      team_name == "Mellow Yellow" ~ "MYL",
      team_name == "Midnight Wisps" ~ "MNW",
      team_name == "Rojo Rollers" ~ "RJR",
      team_name == "Raspberry Racers" ~ "RBR",
      team_name == "Limers" ~ "LMR",
      team_name == "Hornets" ~ "HOR"
    )
  )

cols <- c(
  "#882B1A", "#676564", "#E8751A", "#779AC4", "#646E3F",
  "#9D49B9", "#C09F2F", "#65955B", "#284D95", "#B34525",
  "#FCD306", "#9AD1E8", "#D44F4C", "#BB1A4E", "#A5C254", "#DED32A"
)

df_marbles_rank %>% 
  ggplot(aes(
    x = race_num, 
    y = rank, 
    color = team_name,
    group = team_name
  ))+
  annotate(  #pone en el fondo el texto blanco y la leyenda
    "text",
    x = 4.5,
    y = 13,
    label = "ChilleSnow",
    family = "Oswald",
    fontface = "bold",
    color = "grey18",
    size = 55
  )+
  geom_segment(
    data = tibble(  #crea las lineas para cada competidor
      x = 1,
      xend = 8,
      y = 1:16
    ),
    aes(
      x = x, xend = xend,
      y = y, yend = y
    ),
    color = "grey30",
    size = .15,
    inherit.aes = FALSE
  ) +
  geom_segment( #crea la linea vertical del inicio
    aes(
      x = 1, xend = 1,
      y = 1, yend = 16
    ),
    color = "grey30",
    size = 1
  ) +
  geom_bump( # hace la grafica
    smooth = 7, 
    size = 2.2
  ) +
  geom_point( #crea los circulos del inicio
    data = df_marbles_rank %>% filter(race_num == 1),
    size = 3.5
  ) +
  geom_point( #crea los circulos del final
    data = df_marbles_rank %>% filter(race_num == 8),
    size = 5, 
    shape = 21, 
    fill = "black",
    stroke = 2
  ) +
  geom_text( # pone los nombres de equipo a la derecha
    data = df_marbles_rank %>% filter(race_num == 8),
    aes(
      x = 8.11,
      label = team_abbr
    ),
    family = "Oswald",
    size = 5,
    hjust = 0
  ) +
  geom_text( # pone el numero del ranking a la deracha
    data = df_marbles_rank %>% filter(race_num == 8),
    aes(
      x = 8.58,
      label = rank
    ),
    family = "Oswald",
    size = 5,
    hjust = 1
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous( # en el eje x la leyenda "Race X"
    expand = c(.001, .001),
    limits = c(1, 8.5),
    breaks = 1:8, #debe ser igual a los labels
    labels = c(glue::glue("Race {1:6}"),"Casi", "Finish"),
    sec.axis = dup_axis() # crea el segundo eje
  ) +
  scale_y_reverse(
    expand = c(.03, .03),
    breaks = 1:16 # obliga a que se pinten 16 numero al inicio, sin esto, solo se pintan algunos
  )+
  scale_color_manual(
    values = cols,
    guide = F
  )+
  labs(
    x = "Marbula 1 Season 1: Championship Ranking Over Time",
    caption = "Visualization by Cédric Scherer  •  Data by Jelle's Marble Runs"
  ) 

# esto ayuda a manejar las rutas
# usa el working directory
#ggsave(here::here("plots", "2020_23", "2020_23_MarbleRaces.pdf"), 
#       width = 13, height = 6.5, device = cairo_pdf)


#pdftools::pdf_convert(here::here("plots", "2020_23", "2020_23_MarbleRaces.pdf"),
#                      format = "png", dpi = 350)
