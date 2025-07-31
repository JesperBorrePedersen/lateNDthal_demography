par(pty="s")
#Precip and Mean Temp
bg_data<-na.omit(bg_data)
bg_bio01 <- raster(ncol=551, nrow=551)
values(bg_bio01) <- rep(seq(-30,25,0.1),551)

bg_bio12<-raster(ncol=551, nrow=551)
values(bg_bio12)<-rep(rev(seq(0,2750,5)),551)
values(bg_bio12)<-values(t(bg_bio12))

bg_npp<- raster(ncol=551, nrow=551)
values(bg_npp)<-mean(bg_data$npp)

bg_stack<-stack(bg_bio01,bg_bio12,bg_npp)
names(bg_stack)<-c("bio01","bio12","npp")

bg_pred<-predict(bg_stack,mod.seq,type="logistic")
bg_stack<-stack(bg_pred,bg_stack)

v<-ggplot(as.data.frame(bg_stack),aes(bio01,bio12,z=layer))+theme(aspect.ratio=1)

v +  ylim(0,1750)+xlim(-25,25)+theme_bw()+ geom_contour_filled(alpha=0.8)+
  scale_fill_manual(values=c(rev(terrain.colors(9))))+ 
  geom_point(data=maindata,mapping=aes(x=bio01,y=bio12,z=NA))+
  geom_density_2d(data=bg_data,mapping=aes(x=bio01,y=bio12,z=NA),bins=20,col="black",alpha=0.3)


#Precip and NPP
bg_bio01 <- raster(ncol=551, nrow=551)
values(bg_bio01) <- mean(bg_data$bio01)

bg_bio12<-raster(ncol=551, nrow=551)
values(bg_bio12)<-rep(rev(seq(0,2750,5)),551)
values(bg_bio12)<-values(t(bg_bio12))

bg_npp<- raster(ncol=551, nrow=551)
values(bg_npp)<-rep(seq(0,900,1.635),551)

bg_stack<-stack(bg_bio01,bg_bio12,bg_npp)
names(bg_stack)<-c("bio01","bio12","npp")

bg_pred<-predict(bg_stack,mod.seq,type="logistic")
bg_stack<-stack(bg_pred,bg_stack)


v<-ggplot(as.data.frame(bg_stack),aes(npp,bio12,z=layer))+theme(aspect.ratio=1)
v +  ylim(0,1750)+xlim(0,900)+theme_bw()+ geom_contour_filled(alpha=0.8)+
  scale_fill_manual(values=c(rev(terrain.colors(18))))+ 
  geom_point(data=maindata,mapping=aes(x=npp,y=bio12,z=NA))+
  geom_density_2d(data=bg_data,mapping=aes(x=npp,y=bio12,z=NA),bins=40,col="black",alpha=0.3)


#Mean temp and NPP

bg_bio01 <- raster(ncol=551, nrow=551)
values(bg_bio01) <- rep(rev(seq(-30,25,0.1)),551)
values(bg_bio01)<-values(t(bg_bio01))


bg_bio12<-raster(ncol=551, nrow=551)
values(bg_bio12)<-mean(bg_data$bio12)

bg_npp<- raster(ncol=551, nrow=551)
values(bg_npp)<-rep(seq(0,900,1.635),551)

bg_stack<-stack(bg_bio01,bg_bio12,bg_npp)
names(bg_stack)<-c("bio01","bio12","npp")

bg_pred<-predict(bg_stack,mod.seq,type="logistic")
bg_stack<-stack(bg_pred,bg_stack)


v<-ggplot(as.data.frame(bg_stack),aes(npp,bio01,z=layer))+theme(aspect.ratio=1)
v +  ylim(-25,25)+xlim(0,900)+theme_bw()+ geom_contour_filled(alpha=0.8)+
  scale_fill_manual(values=c(rev(terrain.colors(18))))+ 
  geom_point(data=maindata,mapping=aes(x=npp,y=bio01,z=NA))+
  geom_density_2d(data=bg_data,mapping=aes(x=npp,y=bio01,z=NA),bins=40,col="black",alpha=0.3)
