# Geographical distribution of participants of World championships 2026

library(dplyr)
library(ggplot2)
library(readxl)

setwd("/Users/dsesia/Desktop/case_study_CIO")

data=read.csv("data/woc2026_sprint_Q.csv",sep=",")

data_federations=read_excel("data/federations.xlsx", sheet = 1)

# Add continent info
temp=data_federations[,c("Code","Region")]
all(data$country%in%temp$Code)
colnames(temp)[1]="country"
data=left_join(data,temp)

# extract gender data
data$sex=ifelse(startsWith(data$course,"Men"), "M", "W")
colnames(data)

# Count participants by sex and region
region_pct <- data %>%
  filter(!is.na(sex), !is.na(Region)) %>%
  count(Region, sex, name = "participants") %>%
  group_by(sex) %>%
  mutate(
    percentage = participants / sum(participants) * 100
  ) %>%
  ungroup()

# Count unique countries in each region
region_labels <- data %>%
  filter(!is.na(Region), !is.na(country)) %>%
  distinct(Region, country) %>%
  count(Region, name = "n_countries") %>%
  mutate(
    Region_label = paste0(Region, " (", n_countries, ")")
  )

# Add labels to the plotting dataset
region_pct <- region_pct %>%
  left_join(
    region_labels %>% select(Region, Region_label),
    by = "Region"
  )

# Plot
ggplot(
  region_pct,
  aes(
    x = percentage,
    y = reorder(Region_label, percentage),
    fill = sex
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = participants),
    position = position_dodge(width = 0.8),
    hjust = -0.2,
    size = 3
  ) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(
    values = c("M" = "#264653", "W" = "#e9c46a"),
    labels = c("M" = "Male", "W" = "Female")
  ) +
  labs(
    title = "Regional distribution of athletes",
    subtitle = "Sprint event at the 2026 World Championships",
    x = "Percentage of participants",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 11),
    axis.title.x = element_text(size = 11)
  )
