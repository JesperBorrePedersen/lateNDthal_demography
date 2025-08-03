# Load required libraries
library(ggplot2)
library(reshape2)
library(readxl)
library(dplyr)

table1 <- read_excel("../../1_data/dating/CBS_strict_fixed.xlsx")

table2 <- read_excel("../../1_data/dating/CBS_relaxed_fixed.xlsx")

table3 <- read_excel("../../1_data/dating/CBS_strict_fixed_Thorin_tip_date.xlsx")

table4 <- read_excel("../../1_data/dating/CBS_relaxed_fixed_Thorin_tip_date.xlsx")

table5 <- read_excel("../../1_data/dating/CBS_strict_fixed_LesCottes.xlsx")

# Add an identifier to distinguish the tables
table1$Group <- "Strict"
table2$Group <- "Relaxed"
table3$Group <- "Strict | Thorin tip date"
table4$Group <- "Relaxed | Thorin tip date"
table5$Group <- "Strict | LesCottes no tip date"
# Combine the tables into one
combined_data <- rbind(table1, table2, table3, table4, table5)

# Define the order you want
combined_data$Group <- factor(combined_data$Group, levels = c(
  "Strict", 
  "Relaxed", 
  "Strict | Thorin tip date", 
  "Relaxed | Thorin tip date", 
  "Strict | LesCottes no tip date" 
))

combined_data <- combined_data %>%
  filter(!`Summary Statistic` %in% c("clockRate", "mrca.date-backward(Altai_lineage)", "mrca.date-backward(Chag8_lineage)", "mrca.date-backward(LesCottes_lineage)", "mrca.date-backward(Den17_lineage)", "	mrca.date-backward(Den3_outgroup)", "mrca.date-backward(Fonds_lineage1)", "mrca.date-backward(Late_Neand)", "mrca.date-backward(Modern_humans_lineage)", "mrca.date-backward(Neand_lineage)", "mrca.date-backward(Pes_lineage1)", "mrca.date-backward(Spy_lineage)", "mrca.date-backward(Vind_lineage)", "mrca.date-backward(Den3_outgroup)"))

ggplot(combined_data, aes(x = Group, y = mean, fill = Group, color = Group)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = `lower 95% HPD interval`, ymax = `upper 95% HPD interval`), width = 0.2, color = "black") +
  facet_wrap(~ `Summary Statistic`, scales = "free_y") + 
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    axis.text.x = element_blank(),
    axis.text = element_text(size = 12),          
    axis.title = element_text(size = 14),         
    plot.title = element_text(size = 16, face = "bold"),  
    strip.text = element_text(size = 12),          
    legend.text = element_text(size = 12),        
    legend.title = element_text(size = 14, face = "bold"),  
  ) +
  scale_y_continuous(labels = scales::label_number(sci = FALSE, big.mark = ",")) +
  labs(
    title = "Comparison of Means with 95% HPD Intervals",
    x = "Groups",
    y = "Mean Values"
  ) 
  
write.csv(combined_data, "combined_data.csv", row.names = TRUE)
  