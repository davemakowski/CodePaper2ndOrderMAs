rm()
###############################################################################
#Analysis of datasets generated for assessing the impact of publication bias  #
###############################################################################

################
###Proportion###
################

NumMA=c(3,5,10)
NumS=c(50)
Prop=c(0)
MU=c(-0.69, -0.29, 0)
K2=c(1)

TABsimul=expand.grid(NumMA,NumS,Prop,MU,K2)
names(TABsimul)=c("NumMA","NumS","Prop","MU", "K2")
TABdec=data.frame(dMA2=rep(NA, nrow(TABsimul)), dMAg=rep(NA, nrow(TABsimul)), dMAcv=rep(NA, nrow(TABsimul)), dVote=rep(NA, nrow(TABsimul)), dInd=rep(NA, nrow(TABsimul)))

for (i in (1:nrow(TABsimul))) {
  
  #Name of the file to be read
  Name=TABsimul[i,]
  #PubBiasWeak or PubBiasStrong or PubBiasStrongOpp or Sim
  nameTAB=paste("PubBiasWeak", Name[1], Name[2], Name[3], Name[4], Name[5], ".txt")
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
  
  TABdec$dMA2[i]=sum(Dec_MA2)/length(Dec_MA2)
  TABdec$dMAg[i]=sum(Dec_MAg)/length(Dec_MAg)
  TABdec$dMAcv[i]=sum(Dec_MAcv)/length(Dec_MAcv)
  TABdec$dVote[i]=sum(Dec_Vote)/length(Dec_Vote)
  TABdec$dInd[i]=sum(Dec_Ind)/length(Dec_Ind)
  
}

TABdec=cbind(TABsimul,TABdec)
print(TABdec)

#Proportion of correct conclusion for Vote
dev.new()
par(mfrow=c(2,2))
boxplot(dMAg~MU, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="True mean effect")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("A. REMA                                          ")
boxplot(dMA2~MU, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="True mean effect")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("B. SOMA                                          ")
boxplot(dMAcv~MU, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="True mean effect")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("C. MAMA                                         ")
boxplot(dVote~MU, data=TABdec, range=0, ylim=c(0,1), col="lightblue", ylab="Proportion of correct conclusion", xlab="True mean effect")
abline(h=c(0.75, 0.9), lty=3, col="red")
text(2,0.3, "p<0.0001")
title("D. COMA                                         ")

####################
###RMSE, bias, CI###
####################

NumMA=c(3,5,10)
NumS=c(50)
Prop=c(0)
MU=c(-0.69, -0.29, 0)
K2=c(1)

TABsimul=expand.grid(NumMA,NumS,Prop,MU,K2)
names(TABsimul)=c("NumMA","NumS","Prop","MU", "K2")
TABdec2=data.frame(bMA2=rep(NA, nrow(TABsimul)), bMAg=rep(NA, nrow(TABsimul)), bMAcv=rep(NA, nrow(TABsimul)),rMA2=rep(NA, nrow(TABsimul)), rMAg=rep(NA, nrow(TABsimul)), rMAcv=rep(NA, nrow(TABsimul)), CI_MA2=rep(NA, nrow(TABsimul)), CI_MAg=rep(NA, nrow(TABsimul)), CI_MAcv=rep(NA, nrow(TABsimul)))

for (i in (1:nrow(TABsimul))) {

#Name of the file to be read
Name=TABsimul[i,]
nameTAB=paste("PubBiasWeak", Name[1], Name[2], Name[3], Name[4], Name[5], ".txt")
SimTAB=read.table(nameTAB)

#True MU
MU_i=TABsimul$MU[i]

#RMSE
TABdec2$rMA2[i]=sqrt(mean((SimTAB$MU2-MU_i)^2))
TABdec2$rMAg[i]=sqrt(mean((SimTAB$MUg-MU_i)^2))
TABdec2$rMAcv[i]=sqrt(mean((SimTAB$MUcv-MU_i)^2))

#Bias
TABdec2$bMA2[i]=mean(SimTAB$MU2)-MU_i
TABdec2$bMAg[i]=mean(SimTAB$MUg)-MU_i
TABdec2$bMAcv[i]=mean(SimTAB$MUcv)-MU_i

#Proportions of MU within CI
Dec_MA2=rep(0,length(SimTAB$MU2))
Dec_MAg=Dec_MA2
Dec_MAcv=Dec_MA2

Dec_MA2[MU_i>SimTAB$LB2 & MU_i<SimTAB$UB2]<-1
Dec_MAg[MU_i>SimTAB$LBg & MU_i<SimTAB$UBg]<-1
Dec_MAcv[MU_i>SimTAB$LBcv & MU_i<SimTAB$UBcv]<-1

TABdec2$CI_MA2[i]=sum(Dec_MA2)/length(Dec_MA2)
TABdec2$CI_MAg[i]=sum(Dec_MAg)/length(Dec_MAg)
TABdec2$CI_MAcv[i]=sum(Dec_MAcv)/length(Dec_MAcv)

}

TABdec=cbind(TABdec, TABdec2)
dim(TABdec)
print(TABdec)
write.table(TABdec, file="PubBiasWeak.txt")
