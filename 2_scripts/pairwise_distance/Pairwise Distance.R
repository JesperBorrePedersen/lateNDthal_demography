#### Nucleotide Differences Across Neanderthal Groups ####

# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(ggpubr)

# Function to read and reshape individual Excel files
read_and_pivot <- function(file, group_name) {
  read_excel(file) %>%
    select(-1) %>%  # Remove first column (sample names)
    pivot_longer(cols = everything(), names_to = "Sample", values_to = "Distance") %>%
    drop_na() %>%
    mutate(Group = group_name)
}

# Read and label datasets
Goyet <- read_and_pivot("../../1_data/Pairwise_distance/Goyet_pairwise distance.xls", "Goyet")
Vindija <- read_and_pivot("../../1_data/Pairwise_distance/Vindija_pairwise distance.xls", "Vindija")
Feldhofer <- read_and_pivot("../../1_data/Pairwise_distance/Feldhofer_pairwise distance.xls", "Feldhofer")
LateNeand <- read_and_pivot("../../1_data/Pairwise_distance/LateNeanderthals_pairwise distance.xls", "Late Neanderthals")

# Combine datasets
nuc_diff <- bind_rows(Goyet, Vindija, Feldhofer, LateNeand) %>%
  mutate(
    Group = factor(Group, levels = c("Late Neanderthals", "Vindija", "Goyet", "Feldhofer")),
    Distance = as.numeric(Distance)
  )

# Optional: Export combined data
write.csv(nuc_diff, "Nucleotide_differences.csv", row.names = FALSE)

# Plot
nuc_diff$Group <- factor(nuc_diff$Group, levels = c("Feldhofer", "Vindija", "Goyet", "Late Neanderthals"))

ggplot(nuc_diff, aes(x = Distance, y = Group)) + 
  geom_boxplot(fill = "#0000a258") +
  labs(x = "# nucleotide differences", y = NULL) +
  theme_minimal(base_size = 14) 

#### Significance Testing ####

# Test for normality
shapiro.test(nuc_diff$Distance)

# Wilcoxon tests: Late Neanderthals vs other groups
late <- filter(nuc_diff, Group == "Late Neanderthals")
goyet <- filter(nuc_diff, Group == "Goyet")
vindija <- filter(nuc_diff, Group == "Vindija")
feldhofer <- filter(nuc_diff, Group == "Feldhofer")
others <- filter(nuc_diff, Group != "Late Neanderthals")

# Overall comparison
wilcox.test(late$Distance, others$Distance)

# Pairwise comparisons
p_vals <- c(
  wilcox.test(late$Distance, vindija$Distance)$p.value,
  wilcox.test(late$Distance, goyet$Distance)$p.value,
  wilcox.test(late$Distance, feldhofer$Distance)$p.value
)
print(p_vals)

# Adjust p-values
p.adjust(p_vals, method = "BH")
