####data gen


source("invL.R")


library(MASS)
##change.param=NULL ##no cp

datagen=function(n=200,d=3,s=1,change.param=c(0.3,0.6),rho=0.25){
  ncp=length(change.param); sig=rho**toeplitz(0:(d-1));  x=mvrnorm(n,mu=rep(0,d),sig)
  ##generate arbitrary list of coefficients
  b=list(); for (m in 1:10){
  b[[3*m]]=c(rep(1,s),rep(0,d-2*s),rep(-1,s))
  b[[3*m-1]]=c(rep(-1, s),rep(0,d-s));
  b[[3*m-2]]=c(rep(0, s),rep(1,s),rep(0,d-2*s))}
  ####no cp case
  if(ncp==0){y=c();  p=invlogit(x, b[[1]]); for (i in 1:n){y[i]=rbinom(1,1,p[i])};change.loc=NA}
  if(ncp>=1){y=c(); cun=c(0,floor(change.param*n),n); npart=length(cun)-1
  nsub=c(); for(i in 1:npart){nsub[i]=cun[i+1]-cun[i]}
  p=list(); for(i in 1:npart){xsub=x[(cun[i]+1):(cun[i]+nsub[i]),];  p[[i]]= invlogit(xsub, b[[i]])
  for (j in 1:nsub[i]){y[cun[i]+j]=rbinom(1,1,p[[i]][j])}};  change.loc=cun[-c(1,length(cun))]}  
  data=data.frame(x=x,y=y); out=list(data=data, ncp=ncp,change.loc=change.loc)
  return(out)}
  

# ##example
# ##one cp
# mod.dat=datagen(n=600,d=5,change.param=0.4)
# mod.dat$ncp
# mod.dat$change.loc
# dat=mod.dat$data
# str(dat)
# # 
# ##no cp
# mod.dat=datagen(n=600,d=5,change.param=NULL)
# mod.dat$ncp
# mod.dat$change.loc
# dat=mod.dat$data
# str(dat)
# 
# 
# 
# ##two cp
# mod.dat=datagen(n=600,d=5,change.param=c(0.3,0.6))
# mod.dat$ncp
# mod.dat$change.loc
# dat=mod.dat$data
# str(dat)
# 

