
###################################################################
###Barplots summarizing the results obtained with publication bias#
###################################################################
dev.new()

library(ggplot2)

TAB_Bias_s=read.table("PubBiasStrong.txt", header=T)
TAB_Bias_so=read.table("PubBiasStrongOpp.txt", header=T)
TAB_Bias_w=read.table("PubBiasWeak.txt", header=T)
TAB_Sim=read.table("SimResultsBias.txt", header=T)

N=3

##########RMSE of MES#################

Bias_s=cbind(TAB_Bias_s[TAB_Bias_s$NumMA==N,]$rMA2,TAB_Bias_s[TAB_Bias_s$NumMA==N,]$rMAg,TAB_Bias_s[TAB_Bias_s$NumMA==N,]$rMAcv)
Bias_w=cbind(TAB_Bias_w[TAB_Bias_w$NumMA==N,]$rMA2,TAB_Bias_w[TAB_Bias_w$NumMA==N,]$rMAg,TAB_Bias_w[TAB_Bias_w$NumMA==N,]$rMAcv)
Bias_so=cbind(TAB_Bias_so[TAB_Bias_so$NumMA==N,]$rMA2,TAB_Bias_so[TAB_Bias_so$NumMA==N,]$rMAg,TAB_Bias_so[TAB_Bias_so$NumMA==N,]$rMAcv)
Bias_nb=cbind(TAB_Sim[TAB_Sim$NumMA==N,]$rMA2,TAB_Sim[TAB_Sim$NumMA==N,]$rMAg,TAB_Sim[TAB_Sim$NumMA==N,]$rMAcv)

DATA=rbind(Bias_s,Bias_so,Bias_w,Bias_nb)
DATA=cbind(rep(c('-0.69','-0.29','0'),4),c(rep("Bias 2",3), rep("Bias 3", 3), rep("Bias 1", 3), rep("No bias", 3)), DATA)

#DATA=as.data.frame(DATA)
NameMethod=c(rep("SOMA", dim(DATA)[1]), rep("REMA", dim(DATA)[1]), rep("MAMA", dim(DATA)[1]))
DATA=cbind(NameMethod,rbind(DATA[,1:3], DATA[,c(1,2,4)], DATA[,c(1,2,5)]))
DATA=as.data.frame(DATA)
colnames(DATA)=c("Method","Mu","PublicationBias", "RMSE")
DATA$RMSE=as.numeric(DATA$RMSE)

#DATA_bar=DATA[DATA$Mu==-0.69,]

ggplot(data=DATA, aes(x=Method, y=RMSE, fill=PublicationBias)) + 
  geom_bar(stat="identity", color="black", position=position_dodge())+
  labs(x="Method", y = "RMSE")+facet_wrap(.~Mu, scales = "free")


##########Bias of MES#################

Bias_s=cbind(TAB_Bias_s[TAB_Bias_s$NumMA==N,]$bMA2,TAB_Bias_s[TAB_Bias_s$NumMA==N,]$bMAg,TAB_Bias_s[TAB_Bias_s$NumMA==N,]$bMAcv)
Bias_w=cbind(TAB_Bias_w[TAB_Bias_w$NumMA==N,]$bMA2,TAB_Bias_w[TAB_Bias_w$NumMA==N,]$bMAg,TAB_Bias_w[TAB_Bias_w$NumMA==N,]$bMAcv)
Bias_so=cbind(TAB_Bias_so[TAB_Bias_so$NumMA==N,]$bMA2,TAB_Bias_so[TAB_Bias_so$NumMA==N,]$bMAg,TAB_Bias_so[TAB_Bias_so$NumMA==N,]$bMAcv)
Bias_nb=cbind(TAB_Sim[TAB_Sim$NumMA==N,]$bMA2,TAB_Sim[TAB_Sim$NumMA==N,]$bMAg,TAB_Sim[TAB_Sim$NumMA==N,]$bMAcv)

DATA=rbind(Bias_s,Bias_so,Bias_w,Bias_nb)
DATA=cbind(rep(c('-0.69','-0.29','0'),4),c(rep("Bias 2",3), rep("Bias 3", 3), rep("Bias 1", 3), rep("No bias", 3)), DATA)

#DATA=as.data.frame(DATA)
NameMethod=c(rep("SOMA", dim(DATA)[1]), rep("REMA", dim(DATA)[1]), rep("MAMA", dim(DATA)[1]))
DATA=cbind(NameMethod,rbind(DATA[,1:3], DATA[,c(1,2,4)], DATA[,c(1,2,5)]))
DATA=as.data.frame(DATA)
colnames(DATA)=c("Method","Mu","PublicationBias", "BiasMES")
DATA$BiasMES=as.numeric(DATA$BiasMES)

#DATA_bar=DATA[DATA$Mu==-0.69,]

ggplot(data=DATA, aes(x=Method, y=BiasMES, fill=PublicationBias)) + 
  geom_bar(stat="identity", color="black", position=position_dodge())+
  labs(x="Method", y = "Bias in mean effect size")+
  facet_wrap(.~Mu, scales = "free")




##########Correct decisions#################

Bias_s=cbind(TAB_Bias_s[TAB_Bias_s$NumMA==N,]$dMA2,TAB_Bias_s[TAB_Bias_s$NumMA==N,]$dMAg,TAB_Bias_s[TAB_Bias_s$NumMA==N,]$dMAcv,TAB_Bias_s[TAB_Bias_s$NumMA==N,]$dVote)
Bias_w=cbind(TAB_Bias_w[TAB_Bias_w$NumMA==N,]$dMA2,TAB_Bias_w[TAB_Bias_w$NumMA==N,]$dMAg,TAB_Bias_w[TAB_Bias_w$NumMA==N,]$dMAcv,TAB_Bias_w[TAB_Bias_w$NumMA==N,]$dVote)
Bias_so=cbind(TAB_Bias_so[TAB_Bias_so$NumMA==N,]$dMA2,TAB_Bias_so[TAB_Bias_so$NumMA==N,]$dMAg,TAB_Bias_so[TAB_Bias_so$NumMA==N,]$dMAcv,TAB_Bias_so[TAB_Bias_so$NumMA==N,]$dVote)
Bias_nb=cbind(TAB_Sim[TAB_Sim$NumMA==N,]$dMA2,TAB_Sim[TAB_Sim$NumMA==N,]$dMAg,TAB_Sim[TAB_Sim$NumMA==N,]$dMAcv,TAB_Sim[TAB_Sim$NumMA==N,]$dVote)

DATA=rbind(Bias_s,Bias_so,Bias_w,Bias_nb)
DATA=cbind(rep(c('-0.69','-0.29','0'),4),c(rep("Bias 2",3), rep("Bias 3", 3), rep("Bias 1", 3), rep("No bias", 3)), DATA)

#DATA=as.data.frame(DATA)
NameMethod=c(rep("SOMA", dim(DATA)[1]), rep("REMA", dim(DATA)[1]), rep("MAMA", dim(DATA)[1]), rep("COMA", dim(DATA)[1]))
DATA=cbind(NameMethod,rbind(DATA[,1:3], DATA[,c(1,2,4)], DATA[,c(1,2,5)],DATA[,c(1,2,6)])) 
DATA=as.data.frame(DATA)
colnames(DATA)=c("Method","Mu","PublicationBias", "Prob")
DATA$Prob=1-as.numeric(DATA$Prob)

#DATA_bar=DATA[DATA$Mu==-0.69,]
ggplot(data=DATA, aes(x=Method, y=Prob, fill=PublicationBias)) + 
  geom_bar(stat="identity", color="black", position=position_dodge())+
  labs(x="Method", y = "Proportion of wrong conclusions")+facet_wrap(.~Mu, scales = "free")






