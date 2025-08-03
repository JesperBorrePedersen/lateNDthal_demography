par(pty="s")
#response plots for final model
plot(mod.seq, var="bio01",type = "logistic")
dens<-density(pres$bio01)
lines(dens$x,1-dens$y*2)
dens<-density(na.omit(bg_data$bio01))
lines(dens$x,1-dens$y*2,lty=2,col="gray")

plot(mod.seq, var="bio12",type = "logistic")
dens<-density(pres$bio12)
lines(dens$x,1-dens$y*70)
dens<-density(na.omit(bg_data$bio12))
lines(dens$x,1-dens$y*70,lty=2,col="gray")

plot(mod.seq, var="npp",type = "logistic")
dens<-density(pres$npp)
lines(dens$x,1-dens$y*70)
dens<-density(na.omit(bg_data$npp))
lines(dens$x,1-dens$y*70,lty=2,col="gray")

#ROC plot
plot(dismo::evaluate(p=pres[,-6],a=abs[,-6],model=e@models[[opt.seq$tune.args]]),'ROC')

#evaluation and finding an optimal threshold
sums<-dismo::evaluate(p=pres[,-6],a=abs[,-6],model=e@models[[opt.seq$tune.args]],type="logistic")
sums

#Threshold
threshold<-sums@t[which(sums@TPR+sums@TNR==max(sums@TPR+sums@TNR))]

#Kohen's Kappa
dismo::evaluate(p=pres[,-6],a=abs[,-6],model=e@models[[opt.seq$tune.args]],type="logistic",tr=threshold)@kappa

#True-skills statistic - highest around a threshold of 0.63
(evaluate(p=pres[,-6],a=abs[,-6],model=e@models[[opt.seq$tune.args]],type="logistic",tr=threshold)@TPR + evaluate(p=pres[,-6],a=abs[,-6],model=e@models[[opt.seq$tune.args]],type="logistic",tr=threshold)@TNR) -1
