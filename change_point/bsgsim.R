
source("datgen.R"); source("bsg.R"); source("dist.R")
bsgsim=function(n=300, d=3, s=1, change.param=c(0.3, 0.6), rho=0.25, nsim=100){
  
  mod.dat=list(); dat=list(); mod.bin=list()
  for (j in 1:nsim)  {
    #simulate data
    mod.dat[[j]]=datagen(n=n,d=d,s=s,change.param=change.param,rho=rho)
    dat[[j]]=mod.dat[[j]]$data
    ##run estimators
    mod.bin[[j]]=bsg(dat[[j]])}
  ##Parameter estimates
  estncp=c(); loc.cp=list();
  for (j in 1:nsim){estncp[j]=mod.bin[[j]]$N;loc.cp[[j]]=mod.bin[[j]]$cp}
  hd=c()
  Nd=c()
  tr=floor(change.param*n)
  ncp=length(tr)
  for (j in 1:nsim){
    hd[j]=dist(tr, loc.cp[[j]])
    Nd[j]=abs(ncp-estncp[j])
  }
  # bias in number
  biasN=mean(Nd)
  # bias in locations
  bias=mean(hd)
  # rmse in number
  rmseN=sqrt(mean(Nd^2))
  # rmse in locations
  rmse=sqrt(mean(hd^2))
  fin=data.frame(biasN, rmseN, bias, rmse)
  out=list(fin=fin)
  return(out)}



#sim.mod=bsgsim(n=700,d=3, s=1, change.param=c(0.3, 0.6), nsim=100)
#sim.mod$fin
#sim.mod=bsgsim(n=400,d=3, s=1, change.param=c(0.3, 0.6), nsim=100)




