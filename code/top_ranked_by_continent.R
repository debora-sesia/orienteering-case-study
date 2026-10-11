# Continent distribution of top ranked athletes

library(dplyr)
library(ggplot2)
library(readxl)

setwd("/Users/dsesia/Desktop/case_study_CIO")

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
data_federations=read_excel("data/federations.xlsx", sheet = 1)
data_federations$Region[data_federations$Code=="IIA"]="Independent"

#-------------------------------------------------------------------------------
# MALES
#-------------------------------------------------------------------------------
# 2026 ranking list
#data_ranking=read.csv("data/iof_ranking_MEN_FS_06-10-2026.csv",sep=";")

# 2019 rankist list
data_ranking=read.csv("data/iof_ranking_MEN_FS_31-10-2019.csv",sep=";")

#--------
# Check if all country codes in the ranking file are present in the federations data
all(data_ranking$Country%in%data_federations$Code)

temp=data_federations[,c("Code","Region")]
colnames(temp)[1]="Country"

# Add region/continent name to ranking dataset
data_ranking=left_join(data_ranking,temp)

#--------
# 1. Divide by top... categories
data_ranking <- data_ranking %>%
  mutate(
    top50   = WRS.Position <= 50,
    top100  = WRS.Position <= 100,
    top500  = WRS.Position <= 500,
    top1000 = WRS.Position <= 1000,
    all     = !is.na(WRS.Position)
  )


df_plot <- data_ranking %>%
  pivot_longer(
    cols = c(top50, top100, top500, top1000, all),
    names_to = "ranking",
    values_to = "included"
  ) %>%
  filter(included == TRUE) %>%
  count(ranking, Region, name = "n") %>%
  group_by(ranking) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    ranking = factor(
      ranking,
      levels = c("top50", "top100", "top500", "top1000", "all"),
      labels = c("Top 50", "Top 100", "Top 500", "Top 1000", "All")
    )
  )

ggplot(df_plot, aes(x = ranking, y = proportion, fill = Region)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Top-ranked male athletes",
    x = NULL,
    y = "Proportion of athletes",
    fill = NULL
  ) +
  theme_minimal()+
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = continent_colors)+
  theme(
    plot.title = element_text(size = 13)
  )

#-------------------------------------------------------------------------------
# FEMALES
#-------------------------------------------------------------------------------
# 2026 rankist list
#data_ranking=read.csv("data/iof_ranking_WOMEN_FS_06-10-2026.csv",sep=";")

# 2019 rankist list
data_ranking=read.csv("data/iof_ranking_WOMEN_FS_31-10-2019.csv",sep=";")

#--------
# Check if all country codes in the ranking file are present in the federations data
all(data_ranking$Country%in%data_federations$Code)

temp=data_federations[,c("Code","Region")]
colnames(temp)[1]="Country"

# Add region/continent name to ranking dataset
data_ranking=left_join(data_ranking,temp)

#--------
# 1. Divide by top... categories
data_ranking <- data_ranking %>%
  mutate(
    top50   = WRS.Position <= 50,
    top100  = WRS.Position <= 100,
    top500  = WRS.Position <= 500,
    top1000 = WRS.Position <= 1000,
    all     = !is.na(WRS.Position)
  )


df_plot <- data_ranking %>%
  pivot_longer(
    cols = c(top50, top100, top500, top1000, all),
    names_to = "ranking",
    values_to = "included"
  ) %>%
  filter(included == TRUE) %>%
  count(ranking, Region, name = "n") %>%
  group_by(ranking) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    ranking = factor(
      ranking,
      levels = c("top50", "top100", "top500", "top1000", "all"),
      labels = c("Top 50", "Top 100", "Top 500", "Top 1000", "All")
    )
  )

ggplot(df_plot, aes(x = ranking, y = proportion, fill = Region)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Top-ranked female athletes",
    x = NULL,
    y = "Proportion of athletes",
    fill = NULL
  ) +
  theme_minimal()+
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = continent_colors)+
  theme(
    plot.title = element_text(size = 13)
  )
