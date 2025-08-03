par(pty="s")
#mean temp
lm1<-gam(bio01~s(millenium,k=3),data=maindata)
summary(lm1)


plot(bio01~millenium,maindata)
points(bio01~millenium,bg_data,col="gray",pch=19,cex=.2)
points(bio01~millenium,maindata,pch=19)
pred<-predict(lm1,newdata=data.frame(millenium=1:200000),se.fit=T)
lines(1:200000,pred$fit)
lines(1:200000,pred$fit+1.96*pred$se.fit,lty=2)
lines(1:200000,pred$fit-1.96*pred$se.fit,lty=2)
#bg
lm1<-gam(bio01~s(millenium,k=3),data=bg_data)
pred<-predict(lm1,newdata=data.frame(millenium=1:200000),se.fit=T)
lines(1:200000,pred$fit,col=2)
lines(1:200000,pred$fit+1.96*pred$se.fit,lty=2,col=2)
lines(1:200000,pred$fit-1.96*pred$se.fit,lty=2,col=2)


#mean precip
lm1<-gam(bio12~s(millenium,k=3),data=maindata)
summary(lm1)


plot(bio12~millenium,maindata)
points(bio12~millenium,bg_data,col="gray",pch=19,cex=.2)
points(bio12~millenium,maindata,pch=19)
pred<-predict(lm1,newdata=data.frame(millenium=1:200000),se.fit=T)
lines(1:200000,pred$fit)
lines(1:200000,pred$fit+1.96*pred$se.fit,lty=2)
lines(1:200000,pred$fit-1.96*pred$se.fit,lty=2)
#bg
lm1<-gam(bio12~s(millenium,k=3),data=bg_data)

pred<-predict(lm1,newdata=data.frame(millenium=1:200000),se.fit=T)
lines(1:200000,pred$fit,col=2)
lines(1:200000,pred$fit+1.96*pred$se.fit,lty=2,col=2)
lines(1:200000,pred$fit-1.96*pred$se.fit,lty=2,col=2)


#npp
lm1<-gam(npp~s(millenium,k=3),data=maindata)
summary(lm1)



plot(npp~millenium,maindata)
points(npp~millenium,bg_data,col="gray",pch=19,cex=.2)
points(npp~millenium,maindata,pch=19)
pred<-predict(lm1,newdata=data.frame(millenium=1:200000),se.fit=T)
lines(1:200000,pred$fit)
lines(1:200000,pred$fit+1.96*pred$se.fit,lty=2)
lines(1:200000,pred$fit-1.96*pred$se.fit,lty=2)
#bg
lm1<-gam(npp~s(millenium,k=3),data=bg_data)

pred<-predict(lm1,newdata=data.frame(millenium=1:200000),se.fit=T)
lines(1:200000,pred$fit,col=2)
lines(1:200000,pred$fit+1.96*pred$se.fit,lty=2,col=2)
lines(1:200000,pred$fit-1.96*pred$se.fit,lty=2,col=2)


