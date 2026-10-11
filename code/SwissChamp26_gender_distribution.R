# Swiss Sprint Championships (2026) - Gender distributions among participants

library(dplyr)
library(ggplot2)
library(stringr)

txt="H10                 ( 1.8 km,  30 m, 22 Po.)         16 Teilnehmer
H12                 ( 2.2 km,  35 m, 24 Po.)         31 Teilnehmer
H14                 ( 2.6 km,  55 m, 17 Po.)         59 Teilnehmer
H16                 ( 2.8 km,  55 m, 19 Po.)         64 Teilnehmer
H18                 ( 2.9 km,  65 m, 18 Po.)         51 Teilnehmer
H20                 ( 3.3 km,  75 m, 21 Po.)         28 Teilnehmer
HE                  ( 3.3 km,  75 m, 21 Po.)         82 Teilnehmer
HAL                 ( 2.9 km,  65 m, 18 Po.)         33 Teilnehmer
HAM                 ( 2.4 km,  40 m, 16 Po.)         30 Teilnehmer
HAK                 ( 2.0 km,  30 m, 16 Po.)         29 Teilnehmer
HB                  ( 2.2 km,  35 m, 24 Po.)         14 Teilnehmer
H35                 ( 2.5 km,  45 m, 16 Po.)         11 Teilnehmer
H40                 ( 2.5 km,  45 m, 16 Po.)         15 Teilnehmer
H45                 ( 2.5 km,  45 m, 16 Po.)         40 Teilnehmer
H50                 ( 2.4 km,  45 m, 15 Po.)         43 Teilnehmer
H55                 ( 2.4 km,  45 m, 15 Po.)         50 Teilnehmer
H60                 ( 2.3 km,  40 m, 14 Po.)         56 Teilnehmer
H65                 ( 2.2 km,  45 m, 14 Po.)         47 Teilnehmer
H70                 ( 2.0 km,  30 m, 16 Po.)         39 Teilnehmer
H75                 ( 1.9 km,  30 m, 13 Po.)         24 Teilnehmer
H80                 ( 1.6 km,  25 m, 16 Po.)         20 Teilnehmer
H85                 ( 1.3 km,  25 m, 11 Po.)         12 Teilnehmer
D10                 ( 1.8 km,  30 m, 22 Po.)         10 Teilnehmer
D12                 ( 2.1 km,  35 m, 23 Po.)         45 Teilnehmer
D14                 ( 2.4 km,  50 m, 15 Po.)         53 Teilnehmer
D16                 ( 2.6 km,  55 m, 17 Po.)         48 Teilnehmer
D18                 ( 2.6 km,  60 m, 17 Po.)         40 Teilnehmer
D20                 ( 2.9 km,  70 m, 20 Po.)         27 Teilnehmer
DE                  ( 2.9 km,  70 m, 20 Po.)         39 Teilnehmer
DAL                 ( 2.6 km,  60 m, 17 Po.)         35 Teilnehmer
DAM                 ( 2.2 km,  45 m, 14 Po.)         26 Teilnehmer
DAK                 ( 1.9 km,  30 m, 13 Po.)         47 Teilnehmer
DB                  ( 2.1 km,  35 m, 23 Po.)         26 Teilnehmer
D35                 ( 2.3 km,  40 m, 14 Po.)          8 Teilnehmer
D40                 ( 2.3 km,  40 m, 14 Po.)         21 Teilnehmer
D45                 ( 2.4 km,  40 m, 16 Po.)         38 Teilnehmer
D50                 ( 2.2 km,  45 m, 14 Po.)         46 Teilnehmer
D55                 ( 2.2 km,  30 m, 14 Po.)         42 Teilnehmer
D60                 ( 2.2 km,  30 m, 14 Po.)         32 Teilnehmer
D65                 ( 1.9 km,  30 m, 13 Po.)         26 Teilnehmer
D70                 ( 1.6 km,  25 m, 16 Po.)         23 Teilnehmer
D75                 ( 1.6 km,  25 m, 16 Po.)          8 Teilnehmer
D80                 ( 1.3 km,  25 m, 11 Po.)          6 Teilnehmer"
  
  
# Split the text into separate lines
lines <- str_split(txt, "\n")[[1]]

# Extract the values from each line
df <- tibble(line = lines) %>%
  mutate(
    category = str_extract(line, "^[A-Z0-9]+"),
    distance_km = as.numeric(str_match(line, "\\(\\s*([0-9]+\\.?[0-9]*)\\s*km")[, 2]),
    climb_m = as.numeric(str_match(line, ",\\s*([0-9]+)\\s*m")[, 2]),
    controls = as.integer(str_match(line, ",\\s*([0-9]+)\\s*Po")[, 2]),
    participants = as.integer(str_match(line, "\\)\\s*([0-9]+)\\s+Teilnehmer")[, 2])
  ) %>%
  select(-line)

# Extract gender info
df <- df %>%
  mutate(
    sex = case_when(
      str_starts(category, "H") ~ "Male",
      str_starts(category, "D") ~ "Female",
      TRUE ~ NA_character_
    )
  )


# Aggregate participants by sex 
plot_data <- df %>%
  group_by(sex) %>%
  summarise(
    participants = sum(participants, na.rm = TRUE),
    .groups = "drop"
  )

plot_data$sex=factor(plot_data$sex,levels = c("Male", "Female"))

# Plot
ggplot(plot_data, aes(x = sex, y = participants, fill = sex)) +
  geom_col(colour = NA) +
  geom_text(aes(label = participants), vjust = -0.5, size = 5)+
  scale_x_discrete(labels = c("M" = "Male", "F" = "Female")) +
  scale_fill_manual(
    values = c("Male" = "#264653", "Female" = "#e9c46a")
  ) +
  labs(title = NULL,x = NULL,y = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(size = 10)
  )
