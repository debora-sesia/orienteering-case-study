# Analysis of world ranking events. Where do they take place?

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

#-------------------------------------------------------------------------------

data_events=read_excel("data/world_ranking_events_2006_2030.xlsx",guess_max = 2000)
data_federations=read_excel("data/federations.xlsx", sheet = 1)

# extract year 
data_events <- data_events %>%
  select(-Region) %>%
  mutate(year = as.numeric(paste0("20", str_extract(`Start date`, "^\\d{2}")))) %>%
  mutate(Federation.name = str_split_i(Organiser, ",", 1))

temp=data_federations[,c("Region","Federation.name")]

data_events=left_join(data_events,temp)

#-----------
# focus on Sprint events (optional)
data_events <- data_events %>%
  filter(`Competition format` == "Sprint")
#-----------

data_plot <- data_events %>%
  group_by(year, Region) %>% 
  summarise(n_events = n(), .groups = "drop") %>%
  filter(year <= 2026)

ggplot(data_plot, aes(x = year, y = n_events, color = Region, group = Region)) +
  geom_line(size = 1) +                  
  geom_point(size = 2) +                 
  scale_x_continuous(breaks = unique(data_plot$year)) +
  labs(
    title = "World ranking SPRINT events per region",
    x = "Year",
    y = "Number of events",
    color = "Region"
  ) +
  theme_minimal() +                      
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(linewidth = 0.2),
    panel.grid.major = element_line(color = "#E2E8F0", linewidth = 0.15),
    legend.text = element_text(size = 10), 
  )+
  scale_color_manual(values = continent_colors)
