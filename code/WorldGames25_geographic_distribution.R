# Geographical distribution of participants of World Games 2025

library(dplyr)
library(ggplot2)
library(readxl)
library(tibble)
library(tidyr)
library(scales)
library(stringr)

setwd("/Users/dsesia/Desktop/case_study_CIO")

data_federations=read_excel("data/federations.xlsx", sheet = 1)

text <- 
  "AUS - Australia 2 2 2 2 1 
AUT - Austria 2 2 2 2 1 
BEL - Belgium 1 1 1 1 
BRA - Brazil 2 2 2 2 1 
CAN - Canada 2 2 0 0 0
CHN - People's Republic of China 2 2 2 2 1 
CZE - Czechia 2 2 2 2 1 
ESP - Spain 2 2 2 2 1 
EST - Estonia 0 0 1 1 0 
GBR - Great Britain 2 2 2 2 1 
GER - Germany 2 2 2 2 1 
HKG - Hong Kong, China 2 2 2 2 1 
HUN - Hungary 2 2 2 2 1 
ITA - Italy 2 2 2 2 1 
JPN - Japan 2 2 2 2 1 
LAT - Latvia 2 2 2 2 1 
NZL - New Zealand 2 2 2 2 1 
POL - Poland 2 2 2 2 1 
RSA - South Africa 0 0 1 1 0
SUI - Switzerland 3 3 2 2 1 
SVK - Slovakia 1 1 1 1 0
SWE - Sweden 2 2 2 2 1 
TPE - Chinese Taipei 0 0 1 1 0
UKR - Ukraine 1 1 1 1 0"
  
  
data <- tibble(raw = text) %>%
  # 1. Split into individual rows per country
  mutate(raw = str_split(raw, "(?=\\b[A-Z]{3} - )")) %>%
  unnest(raw) %>%
  filter(raw != "") %>%
  # 2. Extract into Code, Country, and the raw space-separated numbers string
  extract(
    col = raw,
    into = c("code", "country", "numbers"),
    regex = "^([A-Z]{3}) - (.*?)\\s((?:\\d\\s*)+)$",
    remove = TRUE
  ) %>%
  mutate(
    country = str_trim(country),
    numbers = str_trim(numbers)
  ) %>%
  # 3. Dynamically separate the numbers into columns (Col_1, Col_2, etc.)
  separate_wider_delim(
    cols = numbers,
    delim = " ",
    names_sep = "_",
    too_few = "align_start"
  ) %>%
  # 4. Convert numeric columns from text to actual integers
  mutate(across(starts_with("numbers_"), as.integer))

colnames(data)[3:7]=c("n_middle_MEN","n_sprint_MEN","n_middle_WOMEN","n_sprint_WOMEN","n_sprint_relay")

# Add continent info
temp=data_federations[,c("Code","Region")]
all(data$code%in%temp$Code)
colnames(temp)[1]="code"
data=left_join(data,temp)


data_long <- bind_rows(
  data %>%
    pivot_longer(
      cols = c(
        n_middle_MEN, n_sprint_MEN,
        n_middle_WOMEN, n_sprint_WOMEN
      ),
      names_to = c("event", "sex"),
      names_pattern = "n_(middle|sprint)_(MEN|WOMEN)",
      values_to = "n"
    ),
  
  data %>%
    transmute(
      code,
      country,
      Region,
      n = n_sprint_relay,
      event = "relay",
      sex = NA_character_
    )
) %>%
  select(code, country, n, event, sex, Region) %>%
  mutate(
    event = factor(event, levels = c("middle", "sprint", "relay")),
    sex = factor(sex, levels = c("MEN", "WOMEN"))
  )

region_pct_sprint <- data_long %>%
  filter(event == "sprint", !is.na(sex)) %>%
  group_by(sex, Region) %>%
  summarise(
    participations = sum(n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(sex) %>%
  mutate(
    percentage = 100 * participations / sum(participations)
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
region_pct_sprint <- region_pct_sprint %>%
  left_join(
    region_labels %>% select(Region, Region_label),
    by = "Region"
  )

# Plot
ggplot(
  region_pct_sprint,
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
    aes(label = participations),
    position = position_dodge(width = 0.8),
    hjust = -0.2,
    size = 3
  ) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(
    values = c("MEN" = "#264653", "WOMEN" = "#e9c46a"),
    labels = c("MEN" = "Male", "WOMEN" = "Female")
  ) +
  labs(
    title = "Regional distribution of athletes",
    subtitle = "Sprint event at the 2025 World Games",
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
