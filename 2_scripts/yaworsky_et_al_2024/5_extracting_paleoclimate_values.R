###OKay, lets get to extracting values based on time and space!
maindata$millenium<-round(maindata$meanage,-3)
maindata$bio01<-NA
maindata$bio05<-NA
maindata$bio06<-NA
maindata$bio10<-NA
maindata$bio11<-NA
maindata$bio12<-NA
maindata$bio19<-NA
maindata$npp<-NA
maindata$biome<-NA

#setting the range of values we are interested in (50kya-145kya)
allmills<-seq(from=min(maindata$millenium),to=max(maindata$millenium),by=1000)

#table for background points
bg_data<-data.frame("longitude"=NA,"latitude"=NA,"bio01"=NA,"bio05"=NA,"bio06"=NA,"bio10"=NA,"bio11"=NA,"bio12"=NA,"bio19"=NA,"npp"=NA,"biome"=NA,"millenium"=NA)

#the forloop for extracting values
for (i in 1:length(allmills)){
  #selecting the millenium
  Thismil<-allmills[i]
  
  #Selecting the rasters
  climate <- region_slice(time_bp = -(Thismil),
                          bio_variables = c("bio01","bio05","bio06","bio10","bio11","bio12","bio19","npp","biome"),
                          dataset="Krapp2021")
  #make it just Europe
  climate<- terra::crop(climate,terra::ext(region_extent$Europe))
  climate<-terra::project(climate,"epsg:3035")
  
  #background sample first
  #creating random points that are within the mask 
  bg.xy <- dismo::randomPoints(mask=raster(climate[[1]]),n=100)
  Site_vals<-data.frame("longitude"=rep(NA,nrow(bg.xy)),"latitude"=NA,"bio01"=NA,"bio05"=NA,"bio06"=NA,"bio10"=NA,"bio11"=NA,"bio12"=NA,"bio19"=NA,"npp"=NA,"biome"=NA,"millenium"=NA)
  
  #recording lat and long
  Site_vals$longitude<-as.data.frame(bg.xy)$x
  Site_vals$latitude<-as.data.frame(bg.xy)$y
  Site_vals$millenium<-allmills[i]
  #Extracting vales
  extractvals<-as.data.frame(raster::extract(x=climate,y=bg.xy))
  Site_vals$bio01<-extractvals$bio01
  Site_vals$bio05<-extractvals$bio05
  Site_vals$bio06<-extractvals$bio06
  Site_vals$bio10<-extractvals$bio10
  Site_vals$bio11<-extractvals$bio11
  Site_vals$bio12<-extractvals$bio12
  Site_vals$bio19<-extractvals$bio19
  Site_vals$npp<-extractvals$npp
  Site_vals$biome<-extractvals$biome
  
  
  #combining the absence points with the main bg point data.
  bg_data<-rbind(bg_data,Site_vals)
  
  #selecting the sites
  Thesesites<-maindata[maindata$millenium==Thismil,]
  #skipping centuries with no observations
  if (nrow(Thesesites)==0) { next }
  
  
  
  
  #begin extraction
  Thesesites_sp<-st_as_sf(Thesesites,coords=c("X","Y"),crs=OG_proj) # HER HAR JEG AENDRET NOGET!!!!!!!!!!!!!!!!!!!!!!
  Thesesites_sp<-st_transform(Thesesites_sp,lambert)
  
  #extract their values from the rasters
  Site_vals<-as.data.frame(raster::extract(x=climate,y=Thesesites_sp))
  #add them to our original dataframe
  maindata[maindata$millenium==Thismil,]$bio01<-Site_vals$bio01
  maindata[maindata$millenium==Thismil,]$bio05<-Site_vals$bio05
  maindata[maindata$millenium==Thismil,]$bio06<-Site_vals$bio06
  maindata[maindata$millenium==Thismil,]$bio10<-Site_vals$bio10
  maindata[maindata$millenium==Thismil,]$bio11<-Site_vals$bio11
  maindata[maindata$millenium==Thismil,]$bio12<-Site_vals$bio12
  maindata[maindata$millenium==Thismil,]$bio19<-Site_vals$bio19
  maindata[maindata$millenium==Thismil,]$npp<-Site_vals$npp
  maindata[maindata$millenium==Thismil,]$biome<-Site_vals$biome
  
  
  
  
  
  
  gc()
  
}

#coded out for safety
write.csv(maindata,"./1_data/yaworsky_extended/output_data/ArchaeologicalData_v1.csv",row.names=FALSE) #coded out to prevent overwriting
write.csv(bg_data,"./1_data/yaworsky_extended/output_data/AbsencePointData.csv",row.names=FALSE) #coded out to prevent overwriting
