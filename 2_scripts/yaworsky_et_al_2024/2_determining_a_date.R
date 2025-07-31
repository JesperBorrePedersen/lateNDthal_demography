#reading in the location data
#These should bind by GeoLayer
maindata<-read.csv("./1_data/yaworsky_extended/raw_data/FaunalData_52923_NOANIMALS.csv")

#reading in the previous layers
data1<-read.csv("1_data/yaworsky_extended/output_data/Dates_Data/Dates_Geog.csv", na="-")
data2<-read.csv("1_data/yaworsky_extended/output_data/Dates_Data/Dates_ArchLayer.csv", na="-")
data3<-read.csv("1_data/yaworsky_extended/output_data/Dates_Data/Dates_Assemb.csv", na="-")
data3$Age<-as.numeric(data3$Age)
data3$Pos..standard.deviation<-as.numeric(data3$Pos..standard.deviation)
data1$Age<-as.numeric(data1$Age)
data1$Pos..standard.deviation<-as.numeric(data1$Pos..standard.deviation)
data2$Age<-as.numeric(data2$Age)
data2$Pos..standard.deviation<-as.numeric(data2$Pos..standard.deviation)

#adding a column for mean date and sd, although its not really a mean, its more the most likely age with the sum of the distribution within 5000 years
maindata$meanage<-NA
maindata$sdage<-NA
#for loop!
for (i in 1:nrow(maindata)){
  #identifying the site
  site<-maindata[i,]
  #pulling observations from the 3 data tables into 1 table
  dates1<-data1[data1$Locality==site$Locality 
                & data1$Assemblage.name==site$Assemblage
                & data1$GeoLayer==site$GeoLayer.Name,]
  dates2<-data2[data2$Locality==site$Locality 
                & data2$Assemblage.name==site$Assemblage
                & data2$GeoLayer==site$GeoLayer.Name,]
  dates3<-data3[data3$Locality==site$Locality 
                & data3$Assemblage.name==site$Assemblage
                & data3$GeoLayer==site$GeoLayer.Name,]
  dates<-rbind(dates1,dates2,dates3)
  #removing observations where the Age is NA
  dates<-dates[!is.na(dates$Age),]
  dates<-dates[!is.na(dates$Pos..standard.deviation),]
  dates<-dates[dates$Pos..standard.deviation<90000,]
  dates<-dates[dates$Age!=0,]
  #skipping sites with no observation
  if (nrow(dates)== 0){next}
  
  #Now we will only have dated sites! But we still need to deal with calibrating radiocarbon dates so that they are comparable to other methods.
  c14sites<-dates[dates$Dating.method=="14C (radiocarbon) dating",]
  
  # misentered dates have sd's bigger than their age -> therefore they are being removed otherwise calibration won't work
  if (nrow(c14sites)!=0){
  messedupdates<-NA
  for (z in 1:nrow(c14sites)){
    first<-c14sites[z,]
    if(first$Pos..standard.deviation > first$Age) {messedupdates=z}}
  if(is.na(messedupdates) !=T){c14sites<-c14sites[-messedupdates,]}
  }
  #if there are radiocarbon dates, we calibrate them!
  if (nrow(c14sites)!=0){
    cals<-calibrate(x=c14sites$Age,
                    errors=c14sites$Pos..standard.deviation,
                    calCurves = rep("intcal20",nrow(c14sites)),
                    verbose=F)}
  
  #Now we need to know if there are other dates
  notc14sites<-dates[dates$Dating.method!="14C (radiocarbon) dating",]
  udates<-data.frame(year=20000:200000,pdf=0)
  if (nrow(notc14sites)!=0) {
    #stack the distributions and find the mean and sd
    
    for (z in 1:nrow(notc14sites)){
      udates$pdf<-udates$pdf+dnorm(c(20000:200000),notc14sites$Age[z],notc14sites$Pos..standard.deviation[z])
    }
    meanage<-udates$year[udates$pdf==max(udates$pdf)]
    maindata$meanage[i]<-meanage
    maindata$sdage[i]<-sum(udates$pdf[udates$year<=meanage+5000 | udates$year>=meanage-5000])/nrow(notc14sites)
  } 
  
  if (nrow(c14sites)!=0){
    #stack the c14s only and find the mean and sd
    calspd<-as.data.frame(spd(cals,c(200000,20000))$grid)
    #stack calspd and udates
    calspd$totheigh<-calspd$PrDens+udates$pdf
    
    meanage<-calspd$calBP[calspd$totheigh==max(calspd$totheigh)]
    maindata$meanage[i]<-meanage
    maindata$sdage[i]<-sum(calspd$totheigh[calspd$calBP<=meanage+5000 | calspd$calBP>=meanage-5000])/(nrow(c14sites)+nrow(notc14sites))
    #add to mainDF
  }
  
}


write.csv(maindata,"./1_data/yaworsky_extended/output_data/NeanderedgeData.csv")
