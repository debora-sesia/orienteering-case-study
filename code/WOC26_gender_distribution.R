# World championships 2026 - Gender distributions among participants

setwd("/Users/dsesia/Desktop/case_study_CIO")

data=read.csv("data/woc2026_sprint_Q.csv",sep=",")

unique(data$course)
data$sex=ifelse(startsWith(data$course,"Men"), "M", "F")


ggplot(data, aes(x = factor(sex, levels = c("M", "F")), fill = sex)) +
  geom_bar(colour = NA) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    colour = "black"
  ) +
  scale_x_discrete(labels = c("M" = "Male", "F" = "Female")) +
  scale_fill_manual(values = c("M" = "#264653", "F" = "#e9c46a")) +
  labs(title = NULL, x = "Sex", y = NULL) +
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

