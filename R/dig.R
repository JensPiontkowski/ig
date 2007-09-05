`dig` <-
function(x,mu=1.0,lambda=1.0,kernel="normal",parameter.nu=1.0,log=FALSE){
cantidad<-sqrt(lambda/mu)*(sqrt(x/mu)-sqrt(mu/x))
jacobiano<-sqrt(lambda)/sqrt(x^3)
if(kernel=="normal"){
dsty<-dnorm(cantidad,0,1)*jacobiano
}
if(kernel=="t"){
nu<-parameter.nu
dsty<-dt(cantidad,nu)*jacobiano
}
if(kernel=="Laplace"){
dsty<-dLaplace(cantidad)*jacobiano
}
if(kernel=="logistic"){
dsty<-dlogis(cantidad,0,1)*jacobiano
}
if(log==TRUE){dsty<-log(dsty)}
return(dsty)
}

