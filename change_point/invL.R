##invlogit
invlogit=function(x,b){p=exp(x%*%b)/(1+exp(x%*%b))
return(p)}
