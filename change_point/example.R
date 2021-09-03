###example


source("bsg.R")


###NO CP
source("datgen.R")
mod.dat=datagen(n=400,change.param=NULL)
dat=mod.dat$data
mod.dat$change.loc
mod.bs= bsg(dat)
mod.bs



####one cp
source("datgen.R")
mod.dat=datagen(n=400,change.param=c(0.3))
dat=mod.dat$data
mod.dat$change.loc
mod.bs= bsg(dat)
mod.bs





####two cp
source("datgen.R")
mod.dat=datagen(n=400,change.param=c(0.3,0.6))
dat=mod.dat$data
mod.dat$change.loc
mod.bs= bsg(dat)
mod.bs

