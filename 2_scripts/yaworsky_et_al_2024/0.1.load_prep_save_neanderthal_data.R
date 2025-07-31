# Getting data

library(rvest)

# URL for the PHP-script
url_neanderthal <- "https://www.roceeh.uni-tuebingen.de/api/queries_AsiaEurope_140-10KaFaunaNeanderthal2023Ext_CURRENT.php"


#  Read html content
html_content_neanderthal <- read_html(url_neanderthal)


# Extract table
df_neanderthal <- html_content_neanderthal %>%
  html_table(fill = TRUE) %>%
  .[[1]]  # select first table


# Create the new data frame by removing the first row and only keeping the first 15 columns
new_df <- df_neanderthal[-1, 1:15]

# rename columns
colnames(new_df)[c(3, 5, 9, 10, 11, 12, 13, 14, 15)] <- c("GeoLayer.Name", 
                                                                          "Locality.Type", 
                                                                          "Geostrat..Info..age.min",
                                                                          "Geostrat..Info..age.max",
                                                                          "Geostrat..Info..correlation",
                                                                          "Geostrat..Info..mis",
                                                                          "NISP.Sum.class.Mammalia",
                                                                          "NISP.Sum.order.Rodentia",
                                                                          "NISP.Sum.family.Anatidae")


# write csv
write.csv(new_df, "./1_data/yaworsky_extended/raw_data/FaunalData_52923_NOANIMALS.csv", row.names = FALSE)

