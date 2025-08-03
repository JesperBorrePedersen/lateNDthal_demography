#The new Data Columns in our data.
maindata$centrank<-NA
maindata$Suit<-NA


#a little function for reclassifying this to binary using the optimal threshold of max TPR+TNR in the evaluate function
binary_reclassify <- function(x) {
  ifelse(x <=  threshold, 0,
         ifelse(x >  threshold, 1, NA)) 
}

#a dataframe for storing the human niche space measures
allmills<-seq(from=0,to=350000,by=1000)
human_niche<-data.frame(year=allmills,nichesize=NA,meanSuit=NA,meanRankSites=NA,meanSuitSites=NA)



#Predicting across our predictors using a forloop
for (i in 1:length(allmills)){
  
  
  #selecting the century
  Thismil<-allmills[i]
  #selecting the rasters
  
  #Selecting the rasters
  climate <- region_slice(time_bp = -(allmills[i]),
                          bio_variables = c("bio01","bio12","npp","biome"),
                          dataset="Krapp2021")
  #make it just Europe
  climate<- terra::crop(climate,terra::ext(region_extent$Europe))
  climate<-terra::project(climate,"epsg:3035")
  
  #Name of the century for saving
  name<-allmills[i]
  
  #Now to predict over this time slice
  pred <- predict(stack(climate),mod.seq, type = 'logistic')
  #writeRaster(x=pred,filename=paste0("./Results350k/Continuous/",name,".tif"),format="GTiff",overwrite=T)
  
  #extracting data about mean values
  human_niche$meanSuit[i]<-cellStats(pred,"mean",na.rm=T)
  
  #now for a binary map
  ME_binary <- calc(pred, fun=binary_reclassify)
  #writeRaster(x=ME_binary,filename=paste0("./Results350k/Binary/",name,".tif"),format="GTiff",overwrite=T)
  
  #extracting the size of the human niche from the binary
  human_niche$year[i]=name
  human_niche$nichesize[i]=cellStats(ME_binary,sum)
  write.csv(human_niche,"./1_data/yaworsky_extended/output_data/Results350k/Human_Niche_Size_350k.csv",row.names=FALSE)
  
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
  write.csv(human_niche,"./1_data/yaworsky_extended/output_data//Results350k/Human_Niche_Size_350k.csv",row.names=FALSE)
  gc()
}
write.csv(maindata,"./1_data/yaworsky_extended/output_data/Results350k/ArchaeologicalData_v1.csv",row.names=FALSE)




# visualising continous

#visulatizing predictions
toplist<-list.files(path="./1_data/yaworsky_extended/output_data/Results350k/Continuous/")

RD<-stack(paste0("./1_data/yaworsky_extended/output_data/Results350k/Continuous/",rev(toplist[c(order(as.numeric(gsub("[^0-9]", "", toplist))))])))

#shifting the lamber projection from centered at 0 long to 20 long.
lambert_shift<-CRS("+proj=laea +lat_0=52 +lon_0=20 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
RD<-projectRaster(from=RD,crs=lambert_shift)

for (i in 1:length(names(RD))) {
  mill<-as.numeric(gsub("[^0-9]", "", names(RD[[i]])))
  plot(RD[[i]],main=paste0(mill," ka ago"),axes=F,zlim=c(0,1),ylim=c(1200000, 6100000),xlim=c(1350000, 8800000))
  
  icesheet<- get_ice_mask(time_bp = -(mill),
                          dataset="Krapp2021")
  icesheet<-terra::crop(icesheet,terra::ext(region_extent$Europe))
  icesheet<-terra::project(icesheet,lambert_shift)
  plot(icesheet,add=T,col="lightblue",axes=F,alpha=.5,legend = FALSE)
  
  sites<-maindata[maindata$millenium==mill,]
  
  sPDF<-spTransform(sPDF,CRSobj = lambert_shift)
  plot(sPDF,add=T)
  
  if (nrow(sites)<1) {next}
  sites_sp<-st_as_sf(sites,coords=c("longitude","latitude"),crs=crs(OG_proj))
  sites_sp<-st_transform(sites_sp,lambert_shift)
  plot(st_geometry(sites_sp),add=T,pch=19)
}



# visualising binary

#visulatizing predictions
toplist<-list.files(path="./Results350k/Binary/")
RD<-stack(paste0("./Results350k/Binary/",rev(toplist[c(order(as.numeric(gsub("[^0-9]", "", toplist))))])))

#shifting the lamber projection from centered at 0 long to 20 long.
lambert_shift<-CRS("+proj=laea +lat_0=52 +lon_0=20 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
RD<-projectRaster(from=RD,crs=lambert_shift,method="ngb")

for (i in 1:length(names(RD))) {
  mill<-as.numeric(gsub("[^0-9]", "", names(RD[[i]])))
  plot(RD[[i]],main=paste0(mill," ka ago"),axes=F,zlim=c(0,1),ylim=c(1200000, 6100000),xlim=c(1350000, 8800000))
  
  icesheet<- get_ice_mask(time_bp = -(mill),
                          dataset="Krapp2021")
  icesheet<-terra::crop(icesheet,terra::ext(region_extent$Europe))
  icesheet<-terra::project(icesheet,lambert_shift)
  plot(icesheet,add=T,col="lightblue",axes=F,alpha=.5,legend = FALSE)
  
  sites<-maindata[maindata$millenium==mill,]
  
  sPDF<-spTransform(sPDF,CRSobj = lambert_shift)
  plot(sPDF,add=T)
  
  
  if (nrow(sites)<1) {next}
  sites_sp<-st_as_sf(sites,coords=c("longitude","latitude"),crs=crs(OG_proj))
  sites_sp<-st_transform(sites_sp,lambert_shift)
  plot(st_geometry(sites_sp),add=T,pch=19)
}



###########
# Changes in Niche Size
##############


human_niche350k<-read.csv("./1_data/yaworsky_extended/output_data/Results350k/Human_Niche_Size_350k.csv")
human_niche350k$year<-human_niche350k$year*-1
human_niche350k<-merge(human_niche350k,o18,"year")


par(pty="s")
plot(nichesize~year,human_niche350k,type="l",xlab="Year BP",ylab="Niche Size (Cell Count)",main="Neanderthal Climatic Niche",col="gray",lwd=2)
mod<-glm(nichesize~year,data=human_niche350k,family="poisson")
ilink <- family(mod)$linkinv
mod<-gamm(nichesize~s(year,k=8),data=human_niche350k,family="poisson",correlation = corAR1())



pred<-predict(mod$gam,data.frame("year"=-350000:-0),se.fit=T)
lines(-350000:-0,ilink(pred$fit))
lines(-350000:-0,ilink(pred$fit+1.96*pred$se.fit),lty=2)
lines(-350000:-0,ilink(pred$fit-1.96*pred$se.fit),lty=2)

par(new=TRUE)

#Delta 018 will be inversed
plot(-Benthicd18O.permil.~year,human_niche350k,type="l",axes=FALSE,xlab="",ylab="",col=4,lty=2)

mtext("δ18O (per mil)",side=4,line=2.2) 
axis(4, ylim=c(0,40),las=1)
abline(v=10300,lty=2)


summary(mod$gam)
