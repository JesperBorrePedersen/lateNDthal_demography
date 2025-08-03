#The new Data Columns in our data.
maindata$centrank<-NA
maindata$Suit<-NA


#a little function for reclassifying this to binary using the optimal threshold of max TPR+TNR in the evaluate function
binary_reclassify <- function(x) {
  ifelse(x <=  threshold, 0,
         ifelse(x >  threshold, 1, NA)) 
}

#a dataframe for storing the human niche space measures
allmills<-seq(from=30000,to=max(maindata$millenium),by=1000)
human_niche<-data.frame(year=rep(NA,length(allmills)),nichesize=NA,meanSuit=NA,meanRankSites=NA,meanSuitSites=NA)



#Predicting across our predictors using a forloop
for (i in 1:length(allmills)){
  
  
  #selecting the century
  Thismil<-allmills[i]
  #selecting the rasters
  
  #Selecting the rasters
  climate <- region_slice(time_bp = -(Thismil),
                          bio_variables = c("bio01","bio12","npp","biome"),
                          dataset="Krapp2021")
  #make it just Europe
  climate<- terra::crop(climate,terra::ext(region_extent$Europe))
  climate<-terra::project(climate,"epsg:3035")
  
  
  #Name of the century for saving
  name<-allmills[i]
  
  #Now to predict over this time slice
  pred <- dismo::predict(stack(climate),mod.seq, type = 'logistic')
  writeRaster(x=pred,filename=paste0("./1_data/yaworsky_extended/output_data/Neanderthal_Predicted_Niche/Continuous/",name,".tif"),format="GTiff",overwrite=T)
  
  #extracting data about mean values
  human_niche$meanSuit[i]<-cellStats(pred,"mean",na.rm=T)
  
  #now for a binary map
  ME_binary <- calc(pred, fun=binary_reclassify)
  writeRaster(x=ME_binary,filename=paste0("./1_data/yaworsky_extended/output_data/Neanderthal_Predicted_Niche/Binary/",name,".tif"),format="GTiff",overwrite=T)
  
  #extracting the size of the human niche from the binary
  human_niche$year[i]=name
  human_niche$nichesize[i]=cellStats(ME_binary,sum)
  
  ### Now for site specifics
  #The sites
  sites<-maindata[maindata$millenium==name,]
  
  if (nrow(sites)==0){next}
  sites_sp<-st_as_sf(sites,coords=c("X","Y"),crs=projection(OG_proj))
  sites_sp<-st_transform(sites_sp,lambert)
  
  rankedrast<-pred
  values(rankedrast)<-rank(-(values(pred)),na.last="keep")
  #site specific suitability and relative ranking in its century
  maindata$centrank[maindata$millenium==name]<-raster::extract(x=rankedrast,y=sites_sp)
  maindata$Suit[maindata$millenium==name]<-raster::extract(x=pred,y=sites_sp)
  
  #general changes in site ranking and suitability
  human_niche$meanRankSites[i]<-mean(na.omit(maindata$centrank[maindata$millenium==name]))
  human_niche$meanSuitSites[i]<-mean(na.omit(maindata$Suit[maindata$millenium==name]))
  write.csv(human_niche,"./1_data/yaworsky_extended/output_data/Human_Niche_Size.csv",row.names=FALSE)
  gc()
}
write.csv(maindata,"./1_data/yaworsky_extended/output_data/ArchaeologicalData_v2.csv",row.names=FALSE)
