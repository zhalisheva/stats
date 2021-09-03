###SP search


sp.search=function(dat,sp.grid=c(0.25,0.5,0.75)){
  n=dim(dat)[1];  lsp=length(sp.grid); isp.grid=floor(n*sp.grid)
  x=as.matrix(dat[,-which(colnames(dat)=="y")]);
  if(lsp==1){sp=sp.grid}
  if(lsp>1){lik=c()
  for(j in 1:lsp){isp=isp.grid[j];  predat=dat[1:isp,];postdat=dat[-(1:isp),]
    pre.est=estfun(predat)$coef;post.est=estfun(postdat)$coef
    p.pre=invlogit(x,pre.est);p.post=invlogit(x,post.est)
    ###evaluate likelihood
    y=dat$y;lik.pre=c((y*log(p.pre/(1-p.pre)))+ log(1-p.pre))
    lik.post=c((y*log(p.post/(1-p.post)))+ log(1-p.post))
    lik[j]=sum(c(lik.pre[1:isp],lik.post[-(1:isp)]))}
  ##choose best fitting initializer
  sp=sp.grid[which.max(lik)]}; return(list(sp=sp))}
