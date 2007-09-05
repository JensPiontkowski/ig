`wgp` <-
function(u,kernel,nu=1.0){
n<-length(u)
uno<-rep(1,n)
nu<-nu
if(kernel=="normal"){
result<-0*uno
}
if(kernel=="t"){
result<-(nu+1)/(2*((nu+u)^2))
}
if(kernel=="Laplace"){
result<-(1)/(4*sqrt((u^3)))
}
if(kernel=="logistic"){
result<-(((-1)*sqrt(u))+(sinh(sqrt(u))))/(4*sqrt((u^3))*(1+cosh(sqrt(u))))
}
return(result)
}

