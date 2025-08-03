# Load Required Libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)

# Get and Crop World Map
worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')

# Disable s2 geometry for cropping
sf::sf_use_s2(FALSE)

# Define focus region: Eurasia and small region from Africa
eurasia_africa <- st_crop(worldmap, xmin = -10, xmax = 90, ymin = 35, ymax = 72)

# Load Excel Data
sites <- read_excel("../../1_data/Neanderthal_sites_with_genomes.xlsx")
print(head(sites))

# Create Map Plot
ggplot() +
  geom_sf(data = eurasia_africa) +
  geom_point(
    data = sites,
    aes(x = long, y = lat, shape = period, color = site_age, fill = site_age),
    size = 3
  ) +
  scale_color_manual(values = c("old" = "blue", "new" = "red")) +
  scale_fill_manual(values = c("old" = "blue", "new" = "red")) +
  scale_shape_manual(values = c(
    "MIS 3" = 22,              # filled square
    "MIS 4/3" = 14,            # filled diamond
    "MIS 5-4 | MIS 3" = 14,    # filled triangle
    "MIS 5-4" = 0              # hollow square
  )) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    text = element_text(size = 14, family = "Arial"),
    axis.text = element_text(size = 12, family = "Arial"),
    axis.title = element_text(size = 14, family = "Arial"),
    legend.position = "none"
  )

