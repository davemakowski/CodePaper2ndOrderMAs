###########################################
###Generation of datasets (without bias)###
###########################################


NumMA=c(3,5,10)
NumS=c(10,15,25,50)
Prop=c(0, 0.10, 0.25, 0.5)
MU=c(-0.69, -0.29, 0, 0.22,  0.41)
K2=c(0.5)

TABsimul=expand.grid(NumMA,NumS,Prop,MU,K2)

for (l in 1:nrow(TABsimul)) {
Inputs=as.numeric(TABsimul[l,])
print(l)
print(Inputs)
repeat {
Sim=try(SimulMA2(Inputs[1], Inputs[2], Inputs[3], Inputs[4], Inputs[5],runif(1,0,200)), silent=T)
if (is(Sim, 'try-error')!=T) {break}
}
}


#############
###Ponisio###
#############

#NumMA=c(3,5,10)
#NumS=c(10,15,25,50)
#Prop=c(0,0.1,0.25,0.5)
#K2=c(1,1.5)

#TABponi=expand.grid(NumMA,NumS,Prop,K2)

#for (l in 1:nrow(TABponi)) {
#  Inputs=as.numeric(TABponi[l,])
#  print(l)
#  print(Inputs)
#  repeat {
#  Sim=try(SimulPonisio(Inputs[1], Inputs[2], Inputs[3], Inputs[4], runif(1,0,100)),silent=T)
#  if (is(Sim, 'try-error')!=T) {break}
#  }
#}

