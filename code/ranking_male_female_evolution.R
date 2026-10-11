# Number of athletes in the ranking over the years

library(dplyr)
library(ggplot2)

setwd("/Users/dsesia/Desktop/case_study_CIO/data")

# Ranking Men lists 2019:2026
M26=read.csv("iof_ranking_MEN_FS_06-10-2026.csv",sep=";")
M25=read.csv("iof_ranking_MEN_FS_31-10-2025.csv",sep=";")
M24=read.csv("iof_ranking_MEN_FS_31-10-2024.csv",sep=";")
M23=read.csv("iof_ranking_MEN_FS_31-10-2023.csv",sep=";")
M22=read.csv("iof_ranking_MEN_FS_31-10-2022.csv",sep=";")
M21=read.csv("iof_ranking_MEN_FS_31-10-2021.csv",sep=";")
M20=read.csv("iof_ranking_MEN_FS_31-10-2020.csv",sep=";")
M19=read.csv("iof_ranking_MEN_FS_31-10-2019.csv",sep=";")

# Ranking Women lists 2019:2026
W26=read.csv("iof_ranking_WOMEN_FS_06-10-2026.csv",sep=";")
W25=read.csv("iof_ranking_WOMEN_FS_31-10-2025.csv",sep=";")
W24=read.csv("iof_ranking_WOMEN_FS_31-10-2024.csv",sep=";")
W23=read.csv("iof_ranking_WOMEN_FS_31-10-2023.csv",sep=";")
W22=read.csv("iof_ranking_WOMEN_FS_31-10-2022.csv",sep=";")
W21=read.csv("iof_ranking_WOMEN_FS_31-10-2021.csv",sep=";")
W20=read.csv("iof_ranking_WOMEN_FS_31-10-2020.csv",sep=";")
W19=read.csv("iof_ranking_WOMEN_FS_31-10-2019.csv",sep=";")

nM26 <- length(unique(M26$IOF.ID))
nM25 <- length(unique(M25$IOF.ID))
nM24 <- length(unique(M24$IOF.ID))
nM23 <- length(unique(M23$IOF.ID))
nM22 <- length(unique(M22$IOF.ID))
nM21 <- length(unique(M21$IOF.ID))
nM20 <- length(unique(M20$IOF.ID))
nM19 <- length(unique(M19$IOF.ID))

nW26 <- length(unique(W26$IOF.ID))
nW25 <- length(unique(W25$IOF.ID))
nW24 <- length(unique(W24$IOF.ID))
nW23 <- length(unique(W23$IOF.ID))
nW22 <- length(unique(W22$IOF.ID))
nW21 <- length(unique(W21$IOF.ID))
nW20 <- length(unique(W20$IOF.ID))
nW19 <- length(unique(W19$IOF.ID))

df_sex_year <- data.frame(
  year = rep(2019:2026, each = 2),
  sex = rep(c("Male", "Female"), times = 8),
  n = c(
    nM19, nW19,
    nM20, nW20,
    nM21, nW21,
    nM22, nW22,
    nM23, nW23,
    nM24, nW24,
    nM25, nW25,
    nM26, nW26
  )
)

# compute the percentage of men and women
df_sex_year <- df_sex_year %>%
  group_by(year) %>%
  mutate(percentage = round(n / sum(n) * 100, 0)) %>%
  ungroup()

# plot
ggplot(df_sex_year, aes(x = year, y = n, color = sex, group = sex)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("Male" = "#264653", "Female" = "#e9c46a")
  ) +
  scale_x_continuous(breaks = 2019:2026) +
  labs(
    title = NULL,
    x = "Year",
    y = "Number of athletes",
    color = "Sex"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12),
    legend.position = "top"
  )

