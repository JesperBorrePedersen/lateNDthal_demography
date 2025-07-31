maindata<-read.csv("./1_data/yaworsky_extended/output_data/ArchaeologicalData_v1.csv")
maindata<-distinct(maindata,X,Y,millenium,.keep_all=T)
bg_data<-read.csv("./1_data/yaworsky_extended/output_data/AbsencePointData.csv")
bg_data<-na.omit(bg_data)

#getting rid of the observations we can't use because they are outside europe
maindata<-maindata[is.na(maindata$bio01)==F,]

######################################################################
## This small block of code comes from paleoclimate data script ##

# Somehow, this way, it works

maindata_sp<-st_as_sf(maindata,coords=c("X","Y"),crs=projection(sPDF))
OG_proj<-projection(sPDF)

lambert<-CRS("+init=epsg:3035")
sPDF<-spTransform(sPDF,CRSobj = lambert)
projection(sPDF) <- lambert

maindata_sp<-st_transform(maindata_sp,lambert)

########################################################################


plot(st_geometry(maindata_sp),pch=19)
sPDF <- getMap() #World map
plot(sPDF,add=T)


hist(maindata$meanage,breaks=100,xlab="Year BP",ylab="Number of Observations",main="Histogram of Archaeological Contexts")



#looking at variable correlations
cor(na.omit(bg_data[,c(3:11)]))

