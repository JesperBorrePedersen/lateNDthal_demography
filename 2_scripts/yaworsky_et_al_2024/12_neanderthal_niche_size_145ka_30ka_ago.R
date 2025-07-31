human_niche<-read.csv("./1_data/yaworsky_extended/output_data/Human_Niche_Size.csv")
human_niche$year<-human_niche$year*-1
maindata<-read.csv("./1_data/yaworsky_extended/output_data/ArchaeologicalData_v2.csv")
bg_data<-read.csv("./1_data/yaworsky_extended/output_data/AbsencePointData.csv")

#o18
o18<-read.csv("./1_data/yaworsky_extended/raw_data/LisieckiRaymod18O.csv")
o18$year<-o18$Time.ka.*-1000

human_niche<-merge(human_niche,o18,"year")

#each grid cell is 33.45688km2, so we can estimate the total km below
human_niche$sqkm<-human_niche$nichesize*(33.45688^2)


par(pty="s")
plot(nichesize~year,human_niche,type="l",xlab="Year BP",ylab="Niche Size (Cell Count)",main="Neanderthal Climatic Niche",col="gray",lwd=1)
mod<-glm(nichesize~year,data=human_niche,family="poisson")
ilink <- family(mod)$linkinv
mod<-gamm(nichesize~s(year,k=3),data=human_niche,family="poisson",correlation = corAR1())


pred<-predict(mod$gam,data.frame("year"=-145000:-30000),se.fit=T)
lines(-145000:-30000,ilink(pred$fit))
lines(-145000:-30000,ilink(pred$fit+1.96*pred$se.fit),lty=2)
lines(-145000:-30000,ilink(pred$fit-1.96*pred$se.fit),lty=2)


#abline(v=-191000,lty=2,col=4)#mis6
#abline(v=-123000,lty=2,col=2)#mis5e
#abline(v=-109000,lty=2,col=4)#5d
#abline(v=-96000,lty=2,col=2)#5c
#abline(v=-87000,lty=2,col=4)#5b
#abline(v=-82000,lty=2,col=2)#5a
#abline(v=-71000,lty=2,col=4)#mis4
#abline(v=-57000,lty=2,col=2)#mis3
#abline(v=-29000,lty=2,col=4)#mis2

summary(mod$gam)


plot(nichesize~year,human_niche,type="l",xlab="Year BP",ylab="Niche Size (Cell Count)",main="Neanderthal Climatic Niche Size",col="gray",lwd=2)
mod<-glm(nichesize~year,data=human_niche,family="poisson")
ilink <- family(mod)$linkinv
mod<-gamm(nichesize~s(year,k=3),data=human_niche,family="poisson",correlation = corAR1())



pred<-predict(mod$gam,data.frame("year"=-145000:-30000),se.fit=T)
lines(-145000:-30000,ilink(pred$fit))
lines(-145000:-30000,ilink(pred$fit+1.96*pred$se.fit),lty=2)
lines(-145000:-30000,ilink(pred$fit-1.96*pred$se.fit),lty=2)

par(new=TRUE)

#Delta 018 will be inversed
plot(-Benthicd18O.permil.~year,human_niche,type="l",axes=FALSE,xlab="",ylab="",col=4,lty=2)

mtext("δ18O (per mil)",side=4,line=2.2) 
axis(4, ylim=c(0,40),las=1)



plot(nichesize~Benthicd18O.permil.,human_niche,ylab="Niche Size (Cell Count)",xlab="δ18O (per mil)",pch=19)
mod1<-lm(nichesize~Benthicd18O.permil.,human_niche)
abline(mod1)

summary(mod1)


plot(bio12~millenium,aggregate(bg_data,by=list(bg_data$millenium),mean),type="l",ylab="Mean Precip (mm)",xlab="Year BP",main="Estimated Mean Precip Through Time")

abline(h=500,lty=2)
abline(h=1400,lty=2)

