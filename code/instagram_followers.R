# Instagram followers

library(ggplot2)

# Data collected on 10 October 2026
data <- data.frame(
  sport = c(
    "Orienteering",
    "Breaking",
    "Skateboarding",
    "Surfing",
    "Sport Climbing",
    "Cricket",
    "Flag football",
    "Lacrosse",
    "Squash",
    "Baseball/Softball"
  ),
  edition = c(
    "candidate",
    "Paris24",
    "Paris24",
    "Paris24",
    "Paris24",
    "LA28",
    "LA28",
    "LA28",
    "LA28",
    "LA28"
  ),
  official_IG = c(
    "@iof_orienteering",      
    "@breakingforgold",       
    "@worldskatesb",          
    "@isasurfing",            
    "@worldclimbing",         
    "@icc",                   
    "@ifafmedia",             
    "@worldlacrosse",         
    "@worldsquashofficial",   
    "@wbsc"                   
  ),
  n_follower = c(
    22000,       
    219000,    
    253000,   
    288000,   
    397000,   
    32400000, 
    68800,    
    114000,   
    38400,    
    217000    
  )
)


custom_labels <- function(x) {
  ifelse(x >= 1e6, paste0(round(x/1e6, 1), "M"),
         ifelse(x >= 1e3, paste0(round(x/1e3, 0), "K"), as.character(x)))
}

ggplot(data, aes(x = reorder(sport, n_follower), y = n_follower, fill = edition)) +
  geom_bar(stat = "identity", alpha = 1, width = 0.7) +
  scale_y_log10(
    labels = custom_labels,
    expand = expansion(mult = c(0, 0.12)) 
  )+
  coord_flip() +
  geom_text(aes(label = custom_labels(n_follower)), 
            hjust = -0.15, size = 3.5) +
  scale_fill_manual(values = c("candidate" = "#e9c46a", "Paris24" = "#e76f51", "LA28" = "#264653")) +
  labs(
    x = NULL,
    y = "number of IG followers (log-scale)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_text(size = 11)
  )

