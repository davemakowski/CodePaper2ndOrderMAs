rm()
#############################################
#Analysis of datasets (no publication bias) #
#############################################

################
###Proportion###
################

NumMA=c(3,5,10)
NumS=c(10, 15, 25, 50)
Prop=c(0, 0.1, 0.25, 0.50)
MU=c(-0.69, -0.29, 0, 0.22,  0.41)
K2=c(1,0.7, 0.5)

TABsimul=expand.grid(NumMA,NumS,Prop,MU,K2)
names(TABsimul)=c("NumMA","NumS","Prop","MU", "K2")
TABdec=data.frame(MA2=rep(NA, nrow(TABsimul)), MAg=rep(NA, nrow(TABsimul)), MAcv=rep(NA, nrow(TABsimul)), Vote=rep(NA, nrow(TABsimul)), Ind=rep(NA, nrow(TABsimul)))

for (i in (1:nrow(TABsimul))) {
  
  #Name of the file to be read
  Name=TABsimul[i,]
  nameTAB=paste("Sim", Name[1], Name[2], Name[3], Name[4], Name[5], ".txt")
  SimTAB=read.table(nameTAB)
  
  #Proportions of good decision
  Dec_MA2=rep(0,length(SimTAB$MU2))
  Dec_MAg=Dec_MA2
  Dec_MAcv=Dec_MA2
  Dec_Vote=Dec_MA2
  Dec_Ind=Dec_MA2
  
  if (Name[4]<0) {
    Dec_MA2[SimTAB$UB2<0]<-1
    Dec_MAg[SimTAB$UBg<0]<-1
    Dec_MAcv[SimTAB$UBcv<0]<-1
    Dec_Vote[SimTAB$VoteMaj==-1]<-1
    Dec_Ind[SimTAB$VoteMajInd==-1]<-1
  }
  
  if (Name[4]>0) {
    Dec_MA2[SimTAB$LB2>0]<-1
    Dec_MAg[SimTAB$LBg>0]<-1
    Dec_MAcv[SimTAB$LBcv>0]<-1
    Dec_Vote[SimTAB$VoteMaj==1]<-1
    Dec_Ind[SimTAB$VoteMajInd==1]<-1
  }
  
  if (Name[4]==0) {
    Dec_MA2[SimTAB$LB2<0 & SimTAB$UB2>0]<-1
    Dec_MAg[SimTAB$LBg<0 & SimTAB$UBg>0]<-1
    Dec_MAcv[SimTAB$LBcv<0 & SimTAB$UBcv>0]<-1
    Dec_Vote[SimTAB$VoteMaj==0]<-1
    Dec_Ind[SimTAB$VoteMajInd==0]<-1
  }
  
  TABdec$MA2[i]=sum(Dec_MA2)/length(Dec_MA2)
  TABdec$MAg[i]=sum(Dec_MAg)/length(Dec_MAg)
  TABdec$MAcv[i]=sum(Dec_MAcv)/length(Dec_MAcv)
  TABdec$Vote[i]=sum(Dec_Vote)/length(Dec_Vote)
  TABdec$Ind[i]=sum(Dec_Ind)/length(Dec_Ind)
  
}

TABdec=cbind(TABsimul,TABdec)
dim(TABdec)

summary(TABdec[TABdec$Vote<0.8,])
summary(TABdec[TABdec$MA2<0.8,])
summary(TABdec[TABdec$MAcv<0.8,])


#####Stacked####
MATpro=matrix(NA,nrow=4,ncol=5)

Y=TABdec$MAg
P1=length(Y[Y<0.9])/length(Y)
P2=length(Y[Y<0.9 & Y>=0.8])/length(Y)
P3=length(Y[Y<0.8 & Y>=0.7])/length(Y)
P4=length(Y[Y<0.75])/length(Y)
Ptot=c(P1,P2,P3,P4)
MATpro[,1]=Ptot

Y=TABdec$MA2
P1=length(Y[Y<0.9])/length(Y)
P2=length(Y[Y<0.9 & Y>=0.8])/length(Y)
P3=length(Y[Y<0.8 & Y>=0.7])/length(Y)
P4=length(Y[Y<0.75])/length(Y)
Ptot=c(P1,P2,P3,P4)
MATpro[,2]=Ptot

Y=TABdec$MAcv
P1=length(Y[Y<0.9])/length(Y)
P2=length(Y[Y<0.9 & Y>=0.8])/length(Y)
P3=length(Y[Y<0.8 & Y>=0.7])/length(Y)
P4=length(Y[Y<0.75])/length(Y)
Ptot=c(P1,P2,P3,P4)
MATpro[,3]=Ptot

Y=TABdec$Vote
P1=length(Y[Y<0.9])/length(Y)
P2=length(Y[Y<0.9 & Y>=0.8])/length(Y)
P3=length(Y[Y<0.8 & Y>=0.7])/length(Y)
P4=length(Y[Y<0.75])/length(Y)
Ptot=c(P1,P2,P3,P4)
MATpro[,4]=Ptot

Y=TABdec$Ind
P1=length(Y[Y<0.9])/length(Y)
P2=length(Y[Y<0.9 & Y>=0.8])/length(Y)
P3=length(Y[Y<0.8 & Y>=0.7])/length(Y)
P4=length(Y[Y<0.75])/length(Y)
Ptot=c(P1,P2,P3,P4)
MATpro[,5]=Ptot

MATpro=round(MATpro,2)

MethNames=c("REMA", "SOMA", "MAMA", "COMA")
col=c("lightblue","darkgreen", "orange", "red")

dev.new()
par(mfrow=c(1,1))
#barplot(MATpro, names.arg=MethNames, col=col,cex.names=0.6, cex.axis=0.6)
Perf=c(TABdec$MAg, TABdec$MA2, TABdec$MAcv, TABdec$Vote)
Meth=c(rep("A", nrow(TABdec)), rep("B", nrow(TABdec)), rep("C", nrow(TABdec)), rep("D", nrow(TABdec)))
boxplot(Perf~Meth, range=0, xlab="Method", ylab="Proportion of correct conclusion", ylim=c(0,1), names=MethNames, col="lightblue")
abline(h=c(0.75,0.9), col="red", lty=3)
text(1,0.2,paste(MATpro[1,1],", ",MATpro[4,1], sep=""), cex=0.8)
text(2,0.2,paste(MATpro[1,2],", ",MATpro[4,2], sep=""), cex=0.8)
text(3,0.2,paste(MATpro[1,3],", ",MATpro[4,3], sep=""), cex=0.8)
text(4,0.2,paste(MATpro[1,4],", ",MATpro[4,4], sep=""), cex=0.8)
#text(5,0.2,paste(MATpro[1,5],", ",MATpro[4,5], sep=""), cex=0.8)


####GLM###
Mod=glm(cbind(100*MAg, (1-MAg)*100)~NumMA+NumS+Prop+K2+as.factor(MU), data=TABdec, family="binomial")
summary(Mod)
Mod=glm(cbind(100*MA2, (1-MA2)*100)~NumMA+NumS+Prop+K2+as.factor(MU), data=TABdec, family="binomial")
summary(Mod)
Mod=glm(cbind(100*MAcv, (1-MAcv)*100)~NumMA+NumS+Prop+K2+as.factor(MU), data=TABdec, family="binomial")
summary(Mod)
Mod=glm(cbind(100*Vote, (1-Vote)*100)~NumMA+NumS+Prop+K2+as.factor(MU), data=TABdec, family="binomial")
summary(Mod)

####GLM###
Mod=glm(cbind(100*MAg, (1-MAg)*100)~as.factor(MU), data=TABdec, family="binomial")
summary(Mod)
Mod=glm(cbind(100*MA2, (1-MA2)*100)~as.factor(MU), data=TABdec, family="binomial")
summary(Mod)
Mod=glm(cbind(100*MAcv, (1-MAcv)*100)~as.factor(MU), data=TABdec, family="binomial")
summary(Mod)
Mod=glm(cbind(100*Vote, (1-Vote)*100)~as.factor(MU), data=TABdec, family="binomial")
summary(Mod)

####GLM###
Mod=glm(cbind(100*MAg, (1-MAg)*100)~Prop, data=TABdec, family="binomial")
summary(Mod)
Mod=glm(cbind(100*MA2, (1-MA2)*100)~Prop, data=TABdec, family="binomial")
summary(Mod)
Mod=glm(cbind(100*MAcv, (1-MAcv)*100)~Prop, data=TABdec, family="binomial")
summary(Mod)
Mod=glm(cbind(100*Vote, (1-Vote)*100)~Prop, data=TABdec, family="binomial")
summary(Mod)
#Proportion of correct conclusion vs MU
dev.new()
par(mfrow=c(2,2))
boxplot(MAg~MU, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="True mean effect")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("A. REMA                                          ")
boxplot(MA2~MU, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="True mean effect")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("B. SOMA                                          ")
boxplot(MAcv~MU, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="True mean effect")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("C. MAMA                                         ")
boxplot(Vote~MU, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="True mean effect")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("D. COMA                                         ")


#Proportion of correct conclusion vs Prop
dev.new()
par(mfrow=c(2,2))
boxplot(MAg~Prop, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="Proportion of common data")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("A. REMA                                          ")
boxplot(MA2~Prop, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="Proportion of common data")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("B. SOMA                                          ")
boxplot(MAcv~Prop, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="Proportion of common data")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("C. MAMA                                         ")
boxplot(Vote~Prop, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="Proportion of common data")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("D. COMA                                         ")

####################
###RMSE, bias, CI###
####################

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

MethNames=c("REMA", "SOMA", "MAMA", "COMA")
col=c("lightblue","darkgreen", "orange", "red")

MethNames=c("REMA", "SOMA", "MAMA")

dev.new()
par(mfrow=c(3,2))
plot(TABdec$MAg,TABdec$MA2, xlab="REMA", ylab="SOMA", xlim=c(0,0.4), ylim=c(0, 0.4), pch=20)
title("A. RMSE                                        ")
abline(0,1)
abline(v=median(TABdec$MAg), h=median(TABdec$MA2), lty=2, col="red")
plot(TABdec$MAg,TABdec$MAcv, xlab="REMA", ylab="MAMA", xlim=c(0,0.4), ylim=c(0, 0.4), pch=20)
title("B. RMSE                                        ")
abline(0,1)
abline(v=median(TABdec$MAg), h=median(TABdec$MAcv), lty=2, col="red")
plot(TABdec$bMAg,TABdec$bMA2, xlim=c(-0.3,0.2), ylim=c(-0.3,0.2), xlab="REMA", ylab="SOMA", pch=20)
title("C. Bias (Est. - True value)")
abline(0,1)
abline(h=0,v=0, lty=2, col="red")
plot(TABdec$bMAg,TABdec$bMAcv, xlim=c(-0.3,0.2), ylim=c(-0.3,0.2), xlab="REMA", ylab="MAMA", pch=20)
title("D. Bias (Est. - True value)")
abline(0,1)
abline(h=0,v=0, lty=2, col="red")

plot(TABdec$CI_MAg,TABdec$CI_MA2, xlim=c(0.3,1), ylim=c(0.3,1), xlab="REMA", ylab="SOMA", pch=20)
title("E. Coverage of 95%CI                           ")
abline(0,1)
abline(h=0.95,v=0.95, lty=2, col="red")
plot(TABdec$CI_MAg,TABdec$CI_MAcv, xlab="REMA", ylab="MAMA", xlim=c(0.3,1), ylim=c(0.3,1), pch=20)
title("F. Coverage of 95%CI                           ")
abline(0,1)
abline(h=0.95,v=0.95, lty=2, col="red")

summary(glm(log(CI_MA2)~NumMA+NumS+Prop+K2+as.factor(MU), data=TABdec))
summary(glm(log(CI_MAcv)~NumMA+NumS+Prop+K2+as.factor(MU), data=TABdec))
summary(glm(bMAcv~NumMA+NumS+Prop+K2+as.factor(MU), data=TABdec))

dev.new()
par(mfrow=c(3,1))
boxplot(TABdec$CI_MA2~TABdec$Prop, xlab="Proportion of common data", ylab="Coverage of 95%CI", ylim=c(0.4,1), col="lightblue", range=0)
cor.test(TABdec$Prop,TABdec$CI_MA2)
abline(h=0.95, lty=2, col="red")
text(1,0.6, "p<0.001")
title("A. SOMA                                                       ")
boxplot(TABdec$CI_MAcv~TABdec$NumMA, xlab="Number of MA", ylab="Coverage of 95%CI", ylim=c(0.4,1),pch=20, col="lightblue", range=0)
cor.test(TABdec$NumMA,TABdec$CI_MAcv)
abline(h=0.95, lty=2, col="red")
text(0.8,0.6, "p<0.001")
title("B. MAMA                                                      ")
boxplot(TABdec$bMAcv~TABdec$MU, xlab="True mean effect", ylab="Bias (Est. -True value)", ylim=c(-0.3,0.2),pch=20, col="lightblue", range=0)
#cor.test(TABdec$NumS,TABdec$bMAcv)
abline(h=0.95, lty=2, col="red")
text(1, 0.1, "p<0.001")
abline(h=0, lty=2, col="red")
title("C. MAMA                                                     ")
