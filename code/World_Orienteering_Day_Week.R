# World Orienteering Day

# data from https://orienteering.sport/iof/global-development/world-orienteering-day/

library(ggplot2)
library(scales)
library(dplyr)

data <- data.frame(
  year = c(2016:2026),
  participants = c(
    250000, 288675, 349052, 398712, NA,
    135971, 85406, 100952, 119288, 147860, 227539
  ),
  n_countries = c(
    NA, NA, NA, 85, NA,
    51, 65, 63, 73, 72, 68
  ),
  n_events= c(
    NA, NA, NA, 2216, NA,
    841, 615, 780, 974, 1181, 1870
  ),
  notes = c(
    "250,000+", NA, NA, NA, "Cancelled due to COVID-19",
    NA, NA, NA, "1st World Orienteering Week (WOW)", NA, NA
  ),
  type = c(
    rep("day",4),NA,
    rep("day",3),rep("week",3)
  )
)



# Points before and after the missing year
df_dashed <- data %>%
  filter(year %in% c(2019, 2021))

ggplot(data, aes(x = year, y = participants)) +
  geom_line(linewidth = 1, colour = "#264653", na.rm = TRUE) +
  geom_line(
    data = df_dashed,
    aes(x = year, y = participants),
    linewidth = 1,
    colour = "#264653",
    linetype = "dashed"
  ) +
  geom_point(size = 3, colour = "#264653", na.rm = TRUE) +
  scale_x_continuous(breaks = 2016:2026) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  labs(x = "Year", y = "Number of participants") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11)
  )


ggplot(data, aes(x = year, y = n_events)) +
  geom_line(linewidth = 1, colour = "#2a9d8f", na.rm = TRUE) +
  geom_line(
    data = df_dashed,
    aes(x = year, y = n_events),
    linewidth = 1,
    colour = "#2a9d8f",
    linetype = "dashed"
  ) +
  geom_point(size = 3, colour = "#2a9d8f", na.rm = TRUE) +
  scale_x_continuous(breaks = 2016:2026) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  labs(x = "Year", y = "Number of events") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11)
  )


ggplot(data, aes(x = year, y = n_countries)) +
  geom_line(linewidth = 1, colour = "#e76f51", na.rm = TRUE) +
  geom_line(
    data = df_dashed,
    aes(x = year, y = n_countries),
    linewidth = 1,
    colour = "#e76f51",
    linetype = "dashed"
  ) +
  geom_point(size = 3, colour = "#e76f51", na.rm = TRUE) +
  scale_x_continuous(breaks = 2016:2026) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  labs(x = "Year", y = "Number of countries") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11)
  )

#-------------------------------------------------------------------------------
# Add percentage changes to the plots

data <- data %>%
  arrange(year) %>%
  mutate(
    comparison_participants = if_else(
      year == 2021,
      participants[match(2019, year)],
      lag(participants)
    ),
    comparison_countries = if_else(
      year == 2021,
      n_countries[match(2019, year)],
      lag(n_countries)
    ),
    comparison_events = if_else(
      year == 2021,
      n_events[match(2019, year)],
      lag(n_events)
    ),
    pct_participants = (participants / comparison_participants - 1) * 100,
    pct_countries = (n_countries / comparison_countries - 1) * 100,
    pct_events = (n_events / comparison_events - 1) * 100,
    label_participants = if_else(
      is.na(pct_participants), "",
      paste0(if_else(pct_participants > 0, "+", ""),
             round(pct_participants), "%")
    ),
    label_countries = if_else(
      is.na(pct_countries), "",
      paste0(if_else(pct_countries > 0, "+", ""),
             round(pct_countries), "%")
    ),
    label_events = if_else(
      is.na(pct_events), "",
      paste0(if_else(pct_events > 0, "+", ""),
             round(pct_events), "%")
    )
  )

ggplot(data, aes(x = year, y = participants)) +
  geom_line(linewidth = 1, colour = "#264653", na.rm = TRUE) +
  geom_line(
    data = df_dashed,
    aes(x = year, y = participants),
    linewidth = 1,
    colour = "#264653",
    linetype = "dashed"
  ) +
  geom_point(size = 3, colour = "#264653", na.rm = TRUE) +
  geom_text(
    aes(label = label_participants),
    vjust = -1,
    size = 3.5,
    colour = "black",
    na.rm = TRUE
  ) +
  scale_x_continuous(breaks = 2016:2026) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.05, 0.20))
  ) +
  labs(
    x = "Year",
    y = "Number of participants"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11)
  )


ggplot(data, aes(x = year, y = n_events)) +
  geom_line(linewidth = 1, colour = "#2a9d8f", na.rm = TRUE) +
  geom_line(
    data = df_dashed,
    aes(x = year, y = n_events),
    linewidth = 1,
    colour = "#2a9d8f",
    linetype = "dashed"
  ) +
  geom_point(size = 3, colour = "#2a9d8f", na.rm = TRUE) +
  geom_text(
    aes(label = label_events),
    vjust = -1,
    size = 3.5,
    colour = "black",
    na.rm = TRUE
  ) +
  scale_x_continuous(breaks = 2016:2026) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.05, 0.20))
  ) +
  labs(
    x = "Year",
    y = "Number of events"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11)
  )


ggplot(data, aes(x = year, y = n_countries)) +
  geom_line(linewidth = 1, colour = "#e76f51", na.rm = TRUE) +
  geom_line(
    data = df_dashed,
    aes(x = year, y = n_countries),
    linewidth = 1,
    colour = "#e76f51",
    linetype = "dashed"
  ) +
  geom_point(size = 3, colour = "#e76f51", na.rm = TRUE) +
  geom_text(
    aes(label = label_countries),
    vjust = -1,
    size = 3.5,
    colour = "black",
    na.rm = TRUE
  ) +
  scale_x_continuous(breaks = 2016:2026) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.05, 0.20))
  ) +
  labs(
    x = "Year",
    y = "Number of countries"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11)
  )
