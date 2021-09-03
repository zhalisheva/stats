###STEP 1

source("estfun.R")
source("lik_full.R")
source("sp_search.R")


 step1=function(dat,fsp.grid=NULL,bd.sep=0.1,c.bic=0.5,minsep=50){
  ##if length of data is less than minsep then do nothing
  n=dim(dat)[1];if(n<=minsep){cps1=n; fcps1=1;isp=NA; fsp=NA} 
  if(n>minsep){ 
  ##search for best initializer
  if(is.null(fsp.grid)==T){if((0.4*n)<=minsep){fsp.grid=c(0.5)}
      else if((0.25*n)<=minsep & (0.4*n) > minsep){fsp.grid=c(0.4,0.6)}else{
        fsp.grid=c(0.25,0.5,0.75)}}  
  mod.sp=sp.search(dat,sp.grid=fsp.grid);fsp=mod.sp$sp
  ###STEP 0: initial mean estimates
  isp=floor(n*fsp); predat=dat[1:isp,];postdat=dat[-(1:isp),]
  pre.est=estfun(predat)$coef;post.est=estfun(postdat)$coef
  ###STEP 1: update tau
  lik.grid=lik.full(dat,pre.est,post.est)$loss  
  cps1.temp=which.max(lik.grid[-n]); likv=lik.grid[cps1.temp]; likb=lik.grid[n]
  ##check boundary condition
  x=dat[,-which(colnames(dat)=="y")];d=dim(x)[2]
  gamma=c.bic*d*log(n); if(likv-likb<gamma){cps1=n; fcps1=1}else{
  ###check boundary separation
  if(min((cps1.temp/n),1-(cps1.temp/n))<=bd.sep){cps1=n; fcps1=1}else{
  cps1=cps1.temp; fcps1=cps1.temp/n}}} 
  return(list(cp=cps1, fcp=fcps1, sp=isp, fsp=fsp))}

# 
# source("datgen.R")
# mod.dat=datagen(n=35,change.param=c(0.3))
# dat=mod.dat$data
# # 
#  mod=step1(dat)
#  mod
# 
# 
# 
# source("datgen.R")
# mod.dat=datagen(n=300,change.param=NULL)
# dat=mod.dat$data
# 
# mod=step1(dat)
# mod
# 
# 
# source("datgen.R")
# mod.dat=datagen(n=300,change.param=0.2)
# dat=mod.dat$data
# 
# mod=step1(dat)
# mod
# 


