
# Reading in the data
# This Study
maindata1<-read.csv("./1_data/output_data/ArchaeologicalData_v2.csv")
maindata1<-distinct(maindata1,X,Y,millenium,.keep_all=T)

# Yaworsky et al. 2024 study
maindata2<-read.csv("./1_data/yaworsky_et_al_2024/ArchaeologicalData_v1.csv")
maindata2<-distinct(maindata2,longitude,latitude,millenium,.keep_all=T)

# Count observations for legend
n1 <- nrow(maindata1)
n2 <- nrow(maindata2)

# Create legend labels
legend_labels <- c(
  paste0("This Study (n = ", n1, ")"),
  paste0("Yaworsky et al. 2024 (n = ", n2, ")")
)

# For saving histogram as svg
# Open SVG device
svg(
  filename = "3_output/yaworsky_et_al_2024/study_diff_hist.svg",
  width = 12,
  height = 10,
  bg = "white"
)

# Histograms
hist1 <- hist(maindata1$meanage, breaks = 10, plot = FALSE)
hist2 <- hist(maindata2$meanage, breaks = 10, plot = FALSE)

# Plot
plot(hist1, col = rgb(204/255, 121/255, 167/255, 0.5), 
     xlab = "Year BP", ylab = "Observations", 
     main = "Archaeological Contexts")

plot(hist2, col = rgb(153/255, 153/255, 51/255, 0.5), add = TRUE)

# Legend with sample sizes
legend("topright", legend = legend_labels,
       fill = c(rgb(204/255, 121/255, 167/255, 0.5),
                rgb(153/255, 153/255, 51/255, 0.5)))

# Close device
dev.off()

