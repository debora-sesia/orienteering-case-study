# Number of athletes in the ranking over the years

library(dplyr)
library(ggplot2)
library(readxl)
library(dplyr)

setwd("/Users/dsesia/Desktop/case_study_CIO/data")

# Men's rankings: 2019–2026
M26 <- read.csv("iof_ranking_MEN_FS_06-10-2026.csv", sep = ";") %>% mutate(Year = 2026, Gender = "Men")
M25 <- read.csv("iof_ranking_MEN_FS_31-10-2025.csv", sep = ";") %>% mutate(Year = 2025, Gender = "Men")
M24 <- read.csv("iof_ranking_MEN_FS_31-10-2024.csv", sep = ";") %>% mutate(Year = 2024, Gender = "Men")
M23 <- read.csv("iof_ranking_MEN_FS_31-10-2023.csv", sep = ";") %>% mutate(Year = 2023, Gender = "Men")
M22 <- read.csv("iof_ranking_MEN_FS_31-10-2022.csv", sep = ";") %>% mutate(Year = 2022, Gender = "Men")
M21 <- read.csv("iof_ranking_MEN_FS_31-10-2021.csv", sep = ";") %>% mutate(Year = 2021, Gender = "Men")
M20 <- read.csv("iof_ranking_MEN_FS_31-10-2020.csv", sep = ";") %>% mutate(Year = 2020, Gender = "Men")
M19 <- read.csv("iof_ranking_MEN_FS_31-10-2019.csv", sep = ";") %>% mutate(Year = 2019, Gender = "Men")

# Women's rankings: 2019–2026
W26 <- read.csv("iof_ranking_WOMEN_FS_06-10-2026.csv", sep = ";") %>% mutate(Year = 2026, Gender = "Women")
W25 <- read.csv("iof_ranking_WOMEN_FS_31-10-2025.csv", sep = ";") %>% mutate(Year = 2025, Gender = "Women")
W24 <- read.csv("iof_ranking_WOMEN_FS_31-10-2024.csv", sep = ";") %>% mutate(Year = 2024, Gender = "Women")
W23 <- read.csv("iof_ranking_WOMEN_FS_31-10-2023.csv", sep = ";") %>% mutate(Year = 2023, Gender = "Women")
W22 <- read.csv("iof_ranking_WOMEN_FS_31-10-2022.csv", sep = ";") %>% mutate(Year = 2022, Gender = "Women")
W21 <- read.csv("iof_ranking_WOMEN_FS_31-10-2021.csv", sep = ";") %>% mutate(Year = 2021, Gender = "Women")
W20 <- read.csv("iof_ranking_WOMEN_FS_31-10-2020.csv", sep = ";") %>% mutate(Year = 2020, Gender = "Women")
W19 <- read.csv("iof_ranking_WOMEN_FS_31-10-2019.csv", sep = ";") %>% mutate(Year = 2019, Gender = "Women")

# Combine all rankings into one dataframe
data_ranking <- bind_rows(
  M26, M25, M24, M23, M22, M21, M20, M19,
  W26, W25, W24, W23, W22, W21, W20, W19
)
rm(list = setdiff(ls(), "data_ranking"))

#---------
continent_colors <- c(
  "Africa" = "#f4a261",
  "Asia" = "#e9c46a",
  "Europe" = "#264653",
  "North America" = "#287271",
  "South America" = "#2a9d8f",
  "Oceania" = "#e76f51",
  "Independent" = "#C8C8C8"
)

# load federations data
data_federations=read_excel("federations.xlsx", sheet = 1)
data_federations$Region[data_federations$Code=="IIA"]="Independent"

# Check if all country codes in the ranking file are present in the federations data
all(data_ranking$Country%in%data_federations$Code)

temp=data_federations[,c("Code","Region")]
colnames(temp)[1]="Country"

# Add region/continent name to ranking dataset
data_ranking=left_join(data_ranking,temp)
rm(temp)

# 1. Calculate the percentage of athletes by country, year, and gender
region_share <- data_ranking %>%
  filter(!is.na(Region), !is.na(Year), !is.na(Gender)) %>%
  distinct(Year, Gender, Region, IOF.ID) %>%
  group_by(Year, Gender, Region) %>%
  summarise(n_athletes = n_distinct(IOF.ID), .groups = "drop") %>%
  group_by(Year, Gender) %>%
  mutate(
    total_athletes = sum(n_athletes),
    percentage = 100 * n_athletes / total_athletes
  ) %>%
  ungroup()

# 2. Plot for men
data_plot_men <- region_share %>%
  filter(Gender == "Men")

ggplot(data_plot_men,aes(x = Year, y = percentage, color = Region, group = Region)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = 2019:2026) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Continents representation in men's ranking",
    x = "Year",
    y = "Percentage of athletes",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 13)
  )+
  scale_color_manual(values = continent_colors)


# 3. Plot for women
data_plot_women <- region_share %>%
  filter(Gender == "Women")

ggplot(data_plot_women,aes(x = Year, y = percentage, color = Region, group = Region)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = 2019:2026) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Continents representation in women's ranking",
    x = "Year",
    y = "Percentage of athletes",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 13)
  )+
  scale_color_manual(values = continent_colors)
