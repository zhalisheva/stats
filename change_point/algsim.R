#simulations using algorithm with 1 change point

source("datgen.R"); source("alg1.R")
algsim=function(n=300, d=3, s=1, change.param=0.3, rho=0.25, nsim=100){
  mod.dat=list(); dat=list(); mod.alg1=list()
  for (j in 1:nsim)  {
    #simulate data
    mod.dat[[j]]=datagen(n=n,d=d,s=s,change.param=change.param,rho=rho)
    dat[[j]]=mod.dat[[j]]$data
    ##run estimators
    mod.alg1[[j]]=alg1(dat[[j]])}
  ##Parameter estimates
  cp.step1=c(); cp.step2=c(); 
  for (j in 1:nsim){cp.step1[j]=mod.alg1[[j]]$s1$cp;cp.step2[j]=mod.alg1[[j]]$s2$cp}
  tr=floor(change.param*n)
  bias=abs(mean(cp.step2-tr));
  ##RMSE
  rmse=sqrt(mean((cp.step2-tr)^2));
  step2=data.frame(bias, rmse)
  out=list(step2=step2)
  return(out)}

# sim.mod=algsim(n=300,d=15, s=1, change.param=0.8, nsim=100)
# sim.mod



