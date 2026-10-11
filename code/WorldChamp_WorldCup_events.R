# World championships and World Cup events - Where do they take place?

library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)

setwd("/Users/dsesia/Desktop/case_study_CIO")

continent_colors <- c(
  "Africa" = "#f4a261",
  "Asia" = "#e9c46a",
  "Europe" = "#264653",
  "North America" = "#287271",
  "South America" = "#2a9d8f",
  "Oceania" = "#e76f51"
)

data_events_WChamp=read_excel("data/WChamp_events.xlsx",guess_max = 2000)
data_events_WCup=read_excel("data/WCup_events.xlsx",guess_max = 2000)
data_federations=read_excel("data/federations.xlsx", sheet = 1)

temp=data_federations[,c("Region","Federation.name")]

# extract year 
data_events_WChamp <- data_events_WChamp %>%
  select(-Region) %>%
  mutate(year = as.numeric(paste0("20", str_extract(`Start date`, "^\\d{2}")))) %>%
  mutate(Federation.name = str_split_i(Organiser, ",", 1))

data_events_WCup <- data_events_WCup %>%
  select(-Region) %>%
  mutate(year = as.numeric(paste0("20", str_extract(`Start date`, "^\\d{2}")))) %>%
  mutate(Federation.name = str_split_i(Organiser, ",", 1))

data_events_WChamp=left_join(data_events_WChamp,temp)
data_events_WCup=left_join(data_events_WCup,temp)

# merge based on unique region year 
data_events_WChamp_01 <- data_events_WChamp %>%
  select(year, Region) %>%
  distinct() %>%
  mutate(Event = "World Championship")

data_events_WCup_01 <- data_events_WCup %>%
  select(year, Region) %>%
  distinct() %>%
  mutate(Event = "World Cup")

data_plot <- bind_rows(
  data_events_WChamp_01,
  data_events_WCup_01
)

data_plot <- data_plot %>%
  filter(year <= 2026)


region_order <- data_plot %>%
  count(Region, name = "n_events") %>%
  arrange(n_events) %>%
  pull(Region)

region_order <- c(
  "South America",
  "North America",
  "Africa",
  region_order
)

data_plot <- data_plot %>%
  mutate(
    Region = factor(Region, levels = region_order)
  )


# Plot
ggplot(data_plot, aes(x = year, y = Region, color = Region)) +
  geom_point(
    size = 4
  ) +
  facet_wrap(~ Event, ncol = 2) +
  scale_x_continuous(
    breaks = 2006:2026
  ) +
  scale_color_manual(values = continent_colors) +
  labs(
    x = "Year",
    y = "Region"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      color = "#E2E8F0",
      linewidth = 0.3
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )+
  scale_y_discrete(drop = FALSE)+ theme(legend.position="none")
