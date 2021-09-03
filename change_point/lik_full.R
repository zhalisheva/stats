###likelihood full

source("invL.R")

lik.full=function(dat,pre.est,post.est){
  n=dim(dat)[1];x=as.matrix(dat[,-which(colnames(dat)=="y")])
  p.pre=invlogit(x,pre.est);p.post=invlogit(x,post.est)
  y=dat$y;lik.pre=c((y*log(p.pre/(1-p.pre)))+ log(1-p.pre))
  lik.post=c((y*log(p.post/(1-p.post)))+ log(1-p.post))
  mat1=matrix(0,n,n); mat1[upper.tri(mat1,diag=T)]=1;  mat2=1-mat1
  lik.pre.mat=replicate(n,lik.pre);lik.post.mat=replicate(n,lik.post)
  loss.mat=(lik.pre.mat*mat1)+(lik.post.mat*mat2);  loss=(colSums(loss.mat))
  out=list(loss=loss)
  return(out)}
