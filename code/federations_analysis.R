# Geographical distribution and evolution of federations

setwd("/Users/dsesia/Desktop/case_study_CIO")

# install.packages("sf")
# install.packages("rnaturalearth")
library(sf)
library(rnaturalearth)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(knitr)
library(scales)

data_federations=read_excel("data/federations.xlsx", sheet = 1)


status_order <- c("Member", "Provisional member", "Member - suspended", "Provisional member - suspended","Invited")

mat <- data_federations %>%
  count(Region, Status) %>%
  pivot_wider(
    names_from = Status,
    values_from = n,
    values_fill = 0
  ) %>%
  select(Region, all_of(status_order)) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(Total))

# Add totals row at the bottom
mat <- mat %>%
  bind_rows(
    mat %>%
      summarise(
        Region = "Total",
        across(where(is.numeric), sum)
      )
  )

# create table (code for latex)
kable(
  mat,
  format = "latex",
  booktabs = TRUE,
  caption = "Distribution of statuses by region",
  label = "tab:region_status",
  align = "l"
)

# World map
world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

countries <- data_federations %>%
  select(Code, Country, Status, Member.since) %>%
  mutate(present = TRUE)

# standardize the name of the country based on names of rnaturalearth package
countries$name_present=countries$Country%in%world$name_long
countries$Country[countries$Country=="Czechia"]="Czech Republic"
countries$Country[countries$Country=="Great Britain"]="United Kingdom"
countries$Country[countries$Country=="Hong Kong China"]="Hong Kong"
countries$Country[countries$Country=="Iran, Islamic Republic of"]="Iran"
countries$Country[countries$Country=="Korea, Republic of"]="Republic of Korea"
countries$Country[countries$Country=="Moldova, Republic of"]="Moldova"
countries$Country[countries$Country=="Korea, Democratic People's Republic of"]="Dem. Rep. Korea"
countries$Country[countries$Country=="Chinese Taipei"]="Taiwan"
countries$Country[countries$Country=="Turkiye"]="Turkey"


# Join your countries to the world map
world_map <- world %>%
  left_join(
    countries,
    by = c("name_long" = "Country")
  )

# plot colored by member status
unique((world_map$Status))
ggplot(world_map) +
  geom_sf(
    aes(fill = Status),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    name=NULL,
    values = c(
      "NA"                             = "#F1F5F9",  
      "Member"                         = "#191332",  
      "Member - suspended"             = "#6D789A",  
      "Provisional member"             = "#71B48D",  
      "Provisional member - suspended" = "#B4E2BB"   
    ),
    breaks = c(
      "Member", 
      "Member - suspended", 
      "Provisional member", 
      "Provisional member - suspended"
    ),
    na.value = "grey90",
    guide = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 8) 
  )

#-------------------------------------------------------------------------------
# plot colored by date

world_map$Member.since <- as.Date(world_map$Member.since, format = "%d/%m/%Y")

# Plot
ggplot(world_map) +
  geom_sf(
    aes(fill = Member.since),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    name = "Member since: ",                
    option = "viridis",         
    direction = -1,             
    na.value = "#EAEAEA",       
    labels = date_format("%Y"),  
    guide = guide_colorbar(
      barwidth = unit(6, "cm"),  
      barheight = unit(0.3, "cm")
    ),
    transform = "date"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 9)
  )



# Evolution - cumulative number of members

cumulative_data <- world_map %>%
  filter(!is.na(Member.since)) %>%
  mutate(Year = as.numeric(format(as.Date(Member.since), "%Y"))) %>%
  group_by(Year) %>%
  summarise(New_Members = n(), .groups = "drop") %>%
  arrange(Year) %>%
  mutate(Cumulative_Members = cumsum(New_Members))


ggplot(cumulative_data, aes(x = Year, y = Cumulative_Members)) +
  geom_line(color = "#1A365D", linewidth = 0.6, alpha = 0.5) +
  geom_point(color = "#1A365D", size = 2.5, alpha = 0.9) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(1960, max(cumulative_data$Year), by = 10)
  ) +
  scale_y_continuous(
    name = "Total Number of Members"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(size = 13),        
    axis.title = element_text(size = 14),       
    axis.line = element_line(linewidth = 0.5),
    panel.grid.major = element_line(
      color = "#E2E8F0", linewidth = 0.15
    ),
    panel.grid.minor = element_blank()
  )
