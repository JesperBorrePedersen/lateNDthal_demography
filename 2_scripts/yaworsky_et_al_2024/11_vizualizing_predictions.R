#Continous

#visualising predictions
toplist<-list.files(path="./1_data/yaworsky_extended/output_data/Neanderthal_Predicted_Niche/Continuous/")
RD<-stack(paste0("./1_data/yaworsky_extended/output_data/Neanderthal_Predicted_Niche/Continuous/",rev(toplist[c(order(as.numeric(gsub("[^0-9]", "", toplist))))])))

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
  sites_sp<-st_as_sf(sites,coords=c("X","Y"),crs=crs(OG_proj))
  sites_sp<-st_transform(sites_sp,lambert_shift)
  plot(st_geometry(sites_sp),add=T,pch=19)
}



# Binary


#visualising predictions
toplist<-list.files(path="./1_data/yaworsky_extended/output_data/Neanderthal_Predicted_Niche/Binary/")
RD<-stack(paste0("./1_data/yaworsky_extended/output_data/Neanderthal_Predicted_Niche/Binary/",rev(toplist[c(order(as.numeric(gsub("[^0-9]", "", toplist))))])))

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
  sites_sp<-st_as_sf(sites,coords=c("X","Y"),crs=crs(OG_proj))
  sites_sp<-st_transform(sites_sp,lambert_shift)
  plot(st_geometry(sites_sp),add=T,pch=19)
}
