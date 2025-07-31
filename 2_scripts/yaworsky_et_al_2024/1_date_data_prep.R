library(rcarbon)
library(dplyr)
library(rworldmap)
sPDF <- getMap() #World map
library(sf)
library(raster)
library(mgcv)
library(caret)
library(dismo)
#devtools::install_github("EvolEcolGroup/pastclim")
#devtools::install_github("EvolEcolGroup/pastclimData", ref="master")
library(pastclim)
library(pastclimData)
library(ENMeval)
library(viridis)
library(ggplot2)
library(rJava)
#specifying not to write auxillary files with rasters
rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE") # change this as rgdal is not supported any more use sf instead:

sf::gdal_utils("config", c("GDAL_PAM_ENABLED", "FALSE"))

data1<-read.csv("./1_data/yaworsky_extended/raw_data/geolayer.csv",na.strings="", fileEncoding = "latin1")
data1<-data1[,-c(1,5,6,9,11,12,13)]
data2<-read.csv("./1_data/yaworsky_extended/raw_data/archlayer.csv",na.strings="", fileEncoding = "latin1")
data2<-data2[,-c(1,5,6,7,10,12,13,14)]
data3<-read.csv("./1_data/yaworsky_extended/raw_data/assemblage.csv",na.strings="", fileEncoding = "latin1")
data3<-data3[,-c(1,5,6,9,11,12,13)]


#fill in missing merged ones
fill_merged <- function(dat, column){
  ## Get value of each row
  for(n in 1:nrow(dat)){
    ## Check if it is empty
    if(dat[[column]][n] == '' || is.na(dat[[column]][n])){
      dat[[column]][n] <- dat[[column]][n - 1]
    }
  }
  return(dat)
}

#filling in the merged columns with the right information
data1<-fill_merged(data1, "Locality")
data1<-fill_merged(data1, "GeoLayer")
data1<-fill_merged(data1, "Assemblage.name")
#omitting rows with NAs, which should just be empty rows
data1<-na.omit(data1)
#saving
write.csv(data1,"./1_data/yaworsky_extended/output_data/Dates_Data/Dates_Geog.csv")

#data2
#filling in the merged columns with the right information
data2<-fill_merged(data2, "Locality")
data2<-fill_merged(data2, "GeoLayer")
data2<-fill_merged(data2, "Assemblage.name")
data2<-na.omit(data2)
write.csv(data2,"./1_data/yaworsky_extended/output_data/Dates_Data/Dates_ArchLayer.csv")


#data3
#filling in the merged columns with the right information
data3<-fill_merged(data3, "Locality")
data3<-fill_merged(data3, "GeoLayer")
data3<-fill_merged(data3, "Assemblage.name")
data3<-na.omit(data3)
write.csv(data3,"./1_data/yaworsky_extended/output_data/Dates_Data/Dates_Assemb.csv")

