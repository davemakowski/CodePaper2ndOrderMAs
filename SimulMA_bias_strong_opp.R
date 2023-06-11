####Function used to generate data with a bias of type 3####


SimulMA_bias_strong_opp<-function(NumMA,NumS,Prop,MU,K2, Seed) {

library(metafor)
#Seed
#Seed=runif(1,0,100)
set.seed(Seed)

#Number of simulations
N=100
#N=1
RESULTS=matrix(NA,nrow=N,ncol=14)

for (k in 1:N) {

#Number of MAs per simulation
#NumMA=5

#Number of studies per MA
#NumS=50

#Number of studies in common
#Prop=0
NumC=NumS*Prop

#MU
#-0.6931472 -0.2876821 0.0000000 0.2231436  0.4054651
#MU=-0.2876821

#cv
K1=1
#K2=1
#Sigma b
Sigma_b=abs(MU)*K1+0.01

#Max Sigma e
Sigma_e_min=(abs(MU)*K2+0.01)/2
Sigma_e_max=(abs(MU)*K2+0.01)

#Generation of the studies in common
if (NumC>0) {
Theta_c=rnorm(NumC, MU, Sigma_b)
Sigma_e_c=runif(NumC, Sigma_e_min, Sigma_e_max)
Y_c=rnorm(NumC,Theta_c,Sigma_e_c)
}

#Generation of the other studies
NumS_o=NumMA*(NumS-NumC)
Y_o=c()
Sigma_e_o=c()
for (i in 1:NumS_o) {
  repeat {
    #print("test")
    Theta_o=rnorm(1, MU, Sigma_b)
    Sigma_e_oi=runif(1, Sigma_e_min, Sigma_e_max)
    Y_oi=rnorm(1,Theta_o,Sigma_e_oi)
    #print(Y_oi)
    if ((Y_oi-1.96*Sigma_e_oi>0))  {
    #if ((Y_oi+1.96*Sigma_e_oi<0) | (Y_oi-1.96*Sigma_e_oi>0))  {
    #print("ok")
    #print(Y_oi)
      break}
  }
  Y_o=c(Y_o, Y_oi)
  Sigma_e_o=c(Sigma_e_o,Sigma_e_oi)
}

#Storage of results
MU_est=rep(NA,NumMA)
LB=rep(NA,NumMA)
UB=rep(NA,NumMA)
SE=rep(NA,NumMA)

#1st order MA

for (i in 1:NumMA) {
  
  DATA_i=data.frame(Y=Y_o[(NumS*(i-1)+1):(NumS*i)],V=Sigma_e_o[(NumS*(i-1)+1):(NumS*i)]^2)
  if (NumC>0) {
    DATA_i=data.frame(Y=c(Y_c,Y_o[((NumS-NumC)*(i-1)+1):((NumS-NumC)*i)]),V=c(Sigma_e_c^2,Sigma_e_o[((NumS-NumC)*(i-1)+1):((NumS-NumC)*i)]^2))
  }
  
  Mod_i=rma(Y,V,data=DATA_i)
  MU_est[i]=Mod_i$b
  LB[i]=Mod_i$ci.lb
  UB[i]=Mod_i$ci.ub
  SE[i]=Mod_i$se
  #Funnel plot
  #plot(DATA_i$Y,1/DATA_i$V, xlab="Individual effect size", ylab="Accuracy", pch=19, xlim=c(-2, 2))
  #abline(v=MU, lty=1, col="blue", lwd=2)
  #abline(v=MU_est[i], lty=2, col="red", lwd=2)
  #Yi=DATA_i$Y
  #Ai=1/DATA_i$V
  #print(summary(lm(Ai~Yi)))
  #abline(coef(lm(Ai~Yi))[1], coef(lm(Ai~Yi))[2], lwd=2)
  #print(trimfill(Mod_i))
  #plot(trimfill(Mod_i))
    }

#2nd order MA

DATA2=data.frame(Y2=MU_est,V2=SE^2)
#print(DATA2)
Mod.2=rma(Y2,V2,data=DATA2)
MU2=Mod.2$b
LB2=Mod.2$ci.lb
UB2=Mod.2$ci.ub
SE2=Mod.2$se

#Vote counting of 1st order MA
Vote=LB
Vote[LB>0]=1
Vote[UB<0]=(-1)
Vote[LB<0 & UB>0]=0
Pos=length(Vote[Vote==1])
Neg=length(Vote[Vote==(-1)])
NS=length(Vote[Vote==0])
if (Pos > Neg & Pos > NS) VoteMaj=(+1)
if (Neg > Pos & Neg > NS) VoteMaj=(-1)
if (NS >= Neg & NS >= Pos) VoteMaj=0

#Lowest CV
CV=SE/abs(MU_est)
MUcv=MU_est[CV==min(CV)]
LBcv=LB[CV==min(CV)]
UBcv=UB[CV==min(CV)]
SEcv=SE[CV==min(CV)]

#Global MA
DATAg=data.frame(Yg=Y_o, Vg=Sigma_e_o^2)
 if (NumC>0) {
   DATAg=data.frame(Yg=c(Y_c,Y_o), Vg=c(Sigma_e_c^2,Sigma_e_o^2)) 
 }

Mod.g=rma(Yg,Vg,data=DATAg)
MUg=Mod.g$b
LBg=Mod.g$ci.lb
UBg=Mod.g$ci.ub
SEg=Mod.g$se

#Vote counting of individual studies
LB_ind=DATAg$Yg-1.96*sqrt(DATAg$Vg)
UB_ind=DATAg$Yg+1.96*sqrt(DATAg$Vg)
Vote_ind=LB_ind
Vote_ind[LB_ind>0]=1
Vote_ind[UB_ind<0]=(-1)
Vote_ind[LB_ind<0 & UB_ind>0]=0
Pos=length(Vote_ind[Vote_ind==1])
Neg=length(Vote_ind[Vote_ind==(-1)])
NS=length(Vote_ind[Vote_ind==0])
if (Pos > Neg & Pos > NS) VoteMajInd=(+1)
if (Neg > Pos & Neg > NS) VoteMajInd=(-1)
if (NS >= Neg & NS >= Pos) VoteMajInd=0

RESULTS[k,]=c(MU2,LB2,UB2,SE2,MUg,LBg,UBg,SEg,MUcv,LBcv,UBcv,SEcv,VoteMaj,VoteMajInd)

}

RESULTS=as.data.frame(RESULTS)
names(RESULTS)=c("MU2","LB2","UB2","SE2","MUg","LBg","UBg","SEg","MUcv","LBcv","UBcv","SEcv","VoteMaj","VoteMajInd")
print(summary(RESULTS))
nameTAB=paste("PubBiasStrongOpp", NumMA,NumS,Prop,MU,K2, ".txt")
write.table(RESULTS, file=nameTAB)
}



