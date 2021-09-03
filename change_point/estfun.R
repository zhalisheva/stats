###mean estimation function


estfun=function(dat){
   n=dim(dat)[1]
  mod <- glm(y~.-1, family = "binomial", data = dat)
  coef= mod$coefficients
  out <- list(coef=coef)
  return(out)}


# 
# 
# source("datgen.R")
# 
# mod.dat=datagen(n=300,change.param=NULL)
# dat=mod.dat$data
# estfun(dat)
