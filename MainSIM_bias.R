#############################################################
#Simulations of datasets with a bias (the function should be 
#changed depending on the type of bias considered)
#############################################################


NumMA=c(3,5,10)
NumS=c(50)
Prop=c(0)
MU=c(-0.69, -0.29, 0)
K2=c(1)

TABsimul=expand.grid(NumMA,NumS,Prop,MU,K2)

for (l in 1:nrow(TABsimul)) {
Inputs=as.numeric(TABsimul[l,])
print(l)
print(Inputs)
repeat {
Sim=try(SimulMA_bias_strong_opp(Inputs[1], Inputs[2], Inputs[3], Inputs[4], Inputs[5],runif(1,0,200)), silent=T)
if (is(Sim, 'try-error')!=T) {break}
}
}

