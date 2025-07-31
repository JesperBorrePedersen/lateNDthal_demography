sPDF <- getMap() #World map
maindata_sp<-st_as_sf(maindata,coords=c("X","Y"),crs=projection(sPDF))
OG_proj<-projection(sPDF)

lambert<-CRS("+init=epsg:3035")
sPDF<-spTransform(sPDF,CRSobj = lambert)
projection(sPDF) <- lambert

maindata_sp<-st_transform(maindata_sp,lambert)

set_data_path("./1_data/yaworsky_extended/raw_data/Climate")
download_dataset(dataset="Krapp2021", bio_variables = c("bio01","bio05","bio06","bio10","bio11","bio12","bio19","npp","biome"))

