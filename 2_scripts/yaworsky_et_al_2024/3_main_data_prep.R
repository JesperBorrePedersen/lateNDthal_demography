###Important if you want to use this method generally. You should swap out the range (20k and 200k) above to 0 to the max years possible. Right now, dates that are too young or too old are assigned the value of 200k or 20k since there are ties for the max (all of them being 0). You will see that the sd value next to these is 0 or something super low.
#the code below replaces these values with NA
maindata<-read.csv("./1_data/yaworsky_extended/output_data/NeanderedgeData.csv")
maindata$meanage[maindata$meanage==20000 | maindata$meanage==200000]<-NA

#now to remove duplicates!
#these are duplicates in location (long,lat), and time (z). We will need to do this again when they end up binned, I think.
maindata<-distinct(maindata,X,Y,meanage,.keep_all=T)
#and remove NAs
maindata<-maindata[is.na(maindata$meanage)!=T,]

#Trim Temporally - definint the temporal extent
maindata<-maindata[maindata$meanage<145000,]
maindata<-maindata[maindata$meanage>50000,]
hist(maindata$meanage,breaks=100) #this almost looks like I could run with 1000 year intervals
