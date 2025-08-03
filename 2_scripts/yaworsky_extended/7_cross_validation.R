#combining our pres and abs data.
pres<-maindata[,c(6,7,19,24,26,18)]
abs<-bg_data[,c(1,2,3,8,10,12)]
#presabs<-rbind(pres,abs)
#presabs$presabs<-c(rep(1,nrow(maindata)),rep(0,nrow(bg_data)))

# Rename colums "X" and "Y" in "pres" so that it fits with column names in "abs"
pres <- rename(pres, longitude = X, latitude = Y) # Here are changes to the original code


set.seed(12)
##Setting up kfolds
rand <- get.randomkfold(occs=pres,bg=abs,k=10)

#Looking at kfolds
#evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = pres[,-c(1,2,6)], 
#bg.z = abs[,-c(1,2,6)], occs.grp = rand$occs.grp, bg.grp = rand$bg.grp)


#iterate model building over all chosen parameter settings
e <- ENMeval::ENMevaluate(occs = pres[,-6], bg=abs[,-6],
                          tune.args=list(rm = 1:5, fc =  c("L","LQ","LQH","H")), #changed from v4 due to changes to the ENMeval package
                          partitions='user', user.grp=rand, 
                          doClamp = FALSE, algorithm = "maxnet",  
                          parallel = TRUE,  numCores = 14,quiet=T)

e@results



# Overall results
res <- eval.results(e)

# This dplyr operation executes the sequential criteria explained above.
opt.seq <- res %>% 
  filter(delta.AICc <= 2)%>%
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))%>%
  filter(delta.AICc== min(delta.AICc))
opt.seq


par(pty="s")
# We can select a single model from the ENMevaluation object using the tune.args of our
# optimal model and the model that we will use.
mod.seq <- eval.models(e)[[opt.seq$tune.args]]
# Here are the non-zero coefficients in our model.
mod.seq$betas



#Installing maxent.jar if you don't already have it. If this does not work, please find instructions online via google.
utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.4.4/maxent.jar", 
                     destfile = paste0(system.file("java", package = "dismo"), 
                                       "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute


#Variable importance from maxent.jar
ENMeval::ENMevaluate(occs = pres[,-6], bg=abs[,-6],
                     tune.args=list(rm= 1, fc =  c("LQ")),
                     partitions='user', user.grp=rand, 
                     doClamp = FALSE, algorithm = "maxent.jar",  
                     parallel = TRUE,  numCores = 14,quiet=T)@variable.importance
