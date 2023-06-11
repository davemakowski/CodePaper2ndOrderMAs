
##############################################################
####Global analysis of the datasets generated without bias####
##############################################################

NumMA=c(3,5,10)
NumS=c(10, 15, 25, 50)
Prop=c(0, 0.1, 0.25, 0.50)
MU=c(-0.69, -0.29, 0, 0.22,  0.41)
K2=c(1,0.7, 0.5)

TABsimul=expand.grid(NumMA,NumS,Prop,MU,K2)
names(TABsimul)=c("NumMA","NumS","Prop","MU", "K2")
TABdec=data.frame(bMA2=rep(NA, nrow(TABsimul)), bMAg=rep(NA, nrow(TABsimul)), bMAcv=rep(NA, nrow(TABsimul)),MA2=rep(NA, nrow(TABsimul)), MAg=rep(NA, nrow(TABsimul)), MAcv=rep(NA, nrow(TABsimul)), CI_MA2=rep(NA, nrow(TABsimul)), CI_MAg=rep(NA, nrow(TABsimul)), CI_MAcv=rep(NA, nrow(TABsimul)))

for (i in (1:nrow(TABsimul))) {

#Name of the file to be read
Name=TABsimul[i,]
nameTAB=paste("Sim", Name[1], Name[2], Name[3], Name[4], Name[5], ".txt")
SimTAB=read.table(nameTAB)

#True MU
MU_i=TABsimul$MU[i]

#RMSE
TABdec$MA2[i]=sqrt(mean((SimTAB$MU2-MU_i)^2))
TABdec$MAg[i]=sqrt(mean((SimTAB$MUg-MU_i)^2))
TABdec$MAcv[i]=sqrt(mean((SimTAB$MUcv-MU_i)^2))

#Bias
TABdec$bMA2[i]=mean(SimTAB$MU2)-MU_i
TABdec$bMAg[i]=mean(SimTAB$MUg)-MU_i
TABdec$bMAcv[i]=mean(SimTAB$MUcv)-MU_i

#Proportions of MU within CI
Dec_MA2=rep(0,length(SimTAB$MU2))
Dec_MAg=Dec_MA2
Dec_MAcv=Dec_MA2

Dec_MA2[MU_i>SimTAB$LB2 & MU_i<SimTAB$UB2]<-1
Dec_MAg[MU_i>SimTAB$LBg & MU_i<SimTAB$UBg]<-1
Dec_MAcv[MU_i>SimTAB$LBcv & MU_i<SimTAB$UBcv]<-1

TABdec$CI_MA2[i]=sum(Dec_MA2)/length(Dec_MA2)
TABdec$CI_MAg[i]=sum(Dec_MAg)/length(Dec_MAg)
TABdec$CI_MAcv[i]=sum(Dec_MAcv)/length(Dec_MAcv)

}

TABdec=cbind(TABsimul,TABdec)
dim(TABdec)

summary(TABdec)

MethNames=c("Global MA", "2nd order MA", "min CV MA")

dev.new()
par(mfrow=c(1,2))
plot(TABdec$MAg,TABdec$MA2, xlab="Global MA", ylab="2nd order MA", xlim=c(0,0.4), ylim=c(0, 0.4))
title("RMSE")
abline(0,1)
abline(v=median(TABdec$MAg), h=median(TABdec$MA2), lty=2, col="red")
plot(TABdec$MAg,TABdec$MAcv, xlab="Global MA", ylab="Most accurate MA", xlim=c(0,0.4), ylim=c(0, 0.4))
title("RMSE")
abline(0,1)
abline(v=median(TABdec$MAg), h=median(TABdec$MAcv), lty=2, col="red")

dev.new()
par(mfrow=c(1,2))
plot(TABdec$bMAg,TABdec$bMA2, xlim=c(-0.35,0.2), ylim=c(-0.35,0.2), xlab="Global MA", ylab="2nd order MA")
title("Bias (Mean of estimated values - True value)")
abline(0,1)
abline(h=0,v=0, lty=2, col="red")
plot(TABdec$bMAg,TABdec$bMAcv, xlim=c(-0.35,0.2), ylim=c(-0.35,0.2), xlab="Global MA", ylab="Most accurate MA")
title("Bias (Mean of estimated values - True value)")
abline(0,1)
abline(h=0,v=0, lty=2, col="red")

dev.new()
par(mfrow=c(1,2))
plot(TABdec$CI_MAg,TABdec$CI_MA2, xlim=c(0.3,1), ylim=c(0.3,1), xlab="Global MA", ylab="2nd order MA")
title("Coverage of 95%CI")
abline(0,1)
abline(h=0.95,v=0.95, lty=2, col="red")
plot(TABdec$CI_MAg,TABdec$CI_MAcv, xlab="Global MA", ylab="Most accurate MA", xlim=c(0.3,1), ylim=c(0.3,1))
title("Coverage of 95%CI")
abline(0,1)
abline(h=0.95,v=0.95, lty=2, col="red")

TABdec[TABdec$CI_MA2<0.5,]



###########################
####Simulations Ponisio####
###########################

rm()

NumMA=c(3,5,10)
NumS=c(10, 15, 25, 50)
Prop=c(0, 0.1, 0.25, 0.50)
K2=c(1,1.5)

TABsimul=expand.grid(NumMA,NumS,Prop,K2)
names(TABsimul)=c("NumMA","NumS","Prop", "K2")
TABdec=data.frame(bMA2=rep(NA, nrow(TABsimul)), bMAg=rep(NA, nrow(TABsimul)), bMAcv=rep(NA, nrow(TABsimul)),MA2=rep(NA, nrow(TABsimul)), MAg=rep(NA, nrow(TABsimul)), MAcv=rep(NA, nrow(TABsimul)), CI_MA2=rep(NA, nrow(TABsimul)), CI_MAg=rep(NA, nrow(TABsimul)), CI_MAcv=rep(NA, nrow(TABsimul)))

for (i in (1:nrow(TABsimul))) {
  
  #Name of the file to be read
  Name=TABsimul[i,]
  nameTAB=paste("Pon", Name[1], Name[2], Name[3], Name[4], ".txt")
  SimTAB=read.table(nameTAB)
  
  #True MU
  MU_i=-0.29
  
  #RMSE
  TABdec$MA2[i]=sqrt(mean((SimTAB$MU2-MU_i)^2))
  TABdec$MAg[i]=sqrt(mean((SimTAB$MUg-MU_i)^2))
  TABdec$MAcv[i]=sqrt(mean((SimTAB$MUcv-MU_i)^2))
  
  #Bias
  TABdec$bMA2[i]=mean(SimTAB$MU2)-MU_i
  TABdec$bMAg[i]=mean(SimTAB$MUg)-MU_i
  TABdec$bMAcv[i]=mean(SimTAB$MUcv)-MU_i
  
  #Proportions of MU within CI
  Dec_MA2=rep(0,length(SimTAB$MU2))
  Dec_MAg=Dec_MA2
  Dec_MAcv=Dec_MA2
  
  Dec_MA2[MU_i>SimTAB$LB2 & MU_i<SimTAB$UB2]<-1
  Dec_MAg[MU_i>SimTAB$LBg & MU_i<SimTAB$UBg]<-1
  Dec_MAcv[MU_i>SimTAB$LBcv & MU_i<SimTAB$UBcv]<-1
  
  TABdec$CI_MA2[i]=sum(Dec_MA2)/length(Dec_MA2)
  TABdec$CI_MAg[i]=sum(Dec_MAg)/length(Dec_MAg)
  TABdec$CI_MAcv[i]=sum(Dec_MAcv)/length(Dec_MAcv)
  
}

TABdec=cbind(TABsimul,TABdec)
dim(TABdec)

summary(TABdec)

MethNames=c("Global MA", "2nd order MA", "min CV MA")

dev.new()
par(mfrow=c(1,2))
plot(TABdec$MAg,TABdec$MA2, xlab="Global MA", ylab="2nd order MA", xlim=c(0,0.4), ylim=c(0, 0.4))
title("RMSE")
abline(0,1)
abline(v=median(TABdec$MAg), h=median(TABdec$MA2), lty=2, col="red")
plot(TABdec$MAg,TABdec$MAcv, xlab="Global MA", ylab="Most accurate MA", xlim=c(0,0.4), ylim=c(0, 0.4))
title("RMSE")
abline(0,1)
abline(v=median(TABdec$MAg), h=median(TABdec$MAcv), lty=2, col="red")

dev.new()
par(mfrow=c(1,2))
plot(TABdec$bMAg,TABdec$bMA2, xlim=c(-0.2,0.2), ylim=c(-0.2,0.2), xlab="Global MA", ylab="2nd order MA")
title("Bias (Mean of estimated values - True value)")
abline(0,1)
abline(h=0,v=0, lty=2, col="red")
plot(TABdec$bMAg,TABdec$bMAcv, xlim=c(-0.2,0.2), ylim=c(-0.2,0.2), xlab="Global MA", ylab="Most accurate MA")
title("Bias (Mean of estimated values - True value)")
abline(0,1)
abline(h=0,v=0, lty=2, col="red")

dev.new()
par(mfrow=c(1,2))
plot(TABdec$CI_MAg,TABdec$CI_MA2, xlim=c(0.3,1), ylim=c(0.3,1), xlab="Global MA", ylab="2nd order MA")
title("Coverage of 95%CI")
abline(0,1)
abline(h=0.95,v=0.95, lty=2, col="red")
plot(TABdec$CI_MAg,TABdec$CI_MAcv, xlab="Global MA", ylab="Most accurate MA", xlim=c(0.3,1), ylim=c(0.3,1))
title("Coverage of 95%CI")
abline(0,1)
abline(h=0.95,v=0.95, lty=2, col="red")
