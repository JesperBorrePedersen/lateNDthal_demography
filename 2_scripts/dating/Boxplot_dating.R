suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

library(readxl)
data <- read_excel("../../1_data/dating/Boxplot_dating.xlsx")
View(data)

initial <- ggplot(data, aes(x, y, colour = colors)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(xmin = upper, xmax = lower)) +
  scale_color_manual(values = c("Newly generated" = "red", "Radiocarbon Date" = "grey", "Undated" = "black")) +
  scale_y_reverse(position = "right") +
  scale_x_reverse(limits = c(300000,0), labels = scales::comma) +
  ggtitle("Molecular Dating (95% HPD intervals)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time (years)") +
  theme(axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.title=element_blank())

initial + theme(panel.background = element_blank())


