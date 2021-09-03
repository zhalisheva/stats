###binary segmentation

source("alg1.R")

bsg=function(dat,fsp.grid=NULL,bd.sep=0.15,c.bic=0.75,max.itr=10){
n=dim(dat)[1];  obs.indx=1:n; pt=list();cp=list();pt[[1]]=list(obs.indx)
for(m in 1:max.itr){ndp=length(pt[[m]]); mod=list()
for(j in 1:ndp){datsub=dat[pt[[m]][[j]],];mod[[j]]=alg1(datsub,fsp.grid=fsp.grid,bd.sep=bd.sep,c.bic=c.bic)}
counter.b=0;##check if all boundary changes
for(j in 1:ndp){fcp=mod[[j]]$s2$fcp;if(fcp==1){counter.b=counter.b+1}};
if(counter.b==ndp){break}else{counter.nb=0
##if all boundary changes then break else locate changes and update cp list 
## and partition list
cp.temp=list(); pt.temp=list()
for(j in 1:ndp){fcp=mod[[j]]$s2$fcp; if(fcp!=1){counter.nb=counter.nb+1;
#relabel cp wrt current partition
cp.old=mod[[j]]$s2$cp; cp.new=pt[[m]][[j]][cp.old]
##update cp list  
cp.temp=c(cp.temp,list(cp.new));
##update partition list: each new cp yields two new partitions
new.part1=pt[[m]][[j]][pt[[m]][[j]]<=cp.temp[[counter.nb]]]
new.part2=pt[[m]][[j]][pt[[m]][[j]]>cp.temp[[counter.nb]]]
pt.temp=c(pt.temp,list(new.part1),list(new.part2))}}
cp[[m]]=cp.temp; pt[[m+1]]=pt.temp}}
###collect results 
cpv.temp=c();lcp=length(cp); 
if(lcp>0){for(j in 1:lcp){lcpsub=length(cp[[j]]); for(k in 1:lcpsub){cpv.temp=c(cpv.temp,cp[[j]][[k]])}}
cpv=sort(cpv.temp); N.est=length(cpv)}else{cpv=NA;N.est=0}
out=list(cp=cpv,N=N.est)
return(out)}  
  

# 
# ###NO CP
# source("datgen.R")
# mod.dat=datagen(n=400,change.param=NULL)
# dat=mod.dat$data
# mod.dat$change.loc
# mod.bs= bsg(dat)
# mod.bs
# 
# 
# 
# ####one cp
# source("datgen.R")
# mod.dat=datagen(n=400,change.param=c(0.3))
# dat=mod.dat$data
# mod.dat$change.loc
# mod.bs= bsg(dat)
# mod.bs
# 
# 
# 
#   
# 
# ####two cp
# source("datgen.R")
# mod.dat=datagen(n=400,change.param=c(0.3,0.6))
# dat=mod.dat$data
# mod.dat$change.loc
# mod.bs= bsg(dat)
# mod.bs 
#   
#  