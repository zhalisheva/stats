####alg1 with selection

source("step1.R")

alg1=function(dat,fsp.grid=NULL,bd.sep=0.05,c.bic=0.5,minsep=50){
  n=dim(dat)[1];
  mod.s1=step1(dat,fsp.grid=fsp.grid,bd.sep=bd.sep,c.bic=c.bic)
  fcps1=mod.s1$fcp; cps1=mod.s1$cp; 
  ##if step1 at boundary then terminate
  if(fcps1==1){fcps2=1; cps2=n}
  if(fcps1!=1){cps1=mod.s1$cp;predat=dat[1:cps1,];postdat=dat[-(1:cps1),]
  pre.est=estfun(predat)$coef;post.est=estfun(postdat)$coef
  ###STEP 1: update tau
  lik.grid=lik.full(dat,pre.est,post.est)$loss; cps2.temp=which.max(lik.grid[-n]); 
  ###check for boundary separation
  if(min((cps2.temp),(n-(cps2.temp)))<=minsep){cps2=n; fcps2=1}else{cps2=cps2.temp; fcps2=cps2.temp/n}}  
  s1=list(cp=cps1,fcp=fcps1); s2=list(cp=cps2,fcp=fcps2); sp=list(cp=mod.s1$sp, fcp=mod.s1$fsp)
  out=list(s2=s2,s1=s1,sp=sp);return(out)}  
  
  
#
# source("datgen.R")
# mod.dat=datagen(n=300,change.param=c(0.3))
# dat=mod.dat$data
# 
# mod=alg1(dat)
# mod$s2
# mod$s1
# mod$sp
# 
# 
# 
# source("datgen.R")
# mod.dat=datagen(n=300,change.param=NULL)
# dat=mod.dat$data
# 
# mod=alg1(dat)
# mod$s2
# mod$s1
# mod$sp
# #
# 
# #
# source("datgen.R")
# mod.dat=datagen(n=400,change.param=c(0.3,0.8))
# dat=mod.dat$data
# mod.dat$change.loc
# #
# #
# mod=alg1(dat)
# mod$s2
# mod$s1
# mod$sp
# #


#alg1(dat)
