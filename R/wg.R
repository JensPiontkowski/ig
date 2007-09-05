`wg` <-
function(u,kernel,nu=1.0){
n<-length(u)
uno<-rep(1,n)
nu<-nu
if(kernel=="normal"){
result<-(-1/2)*uno
}
if(kernel=="t"){
result<-((-1)*(nu+1))/(2*(nu+u))
}
if(kernel=="Laplace"){
result<-(-1)/(2*sqrt(u))
}
if(kernel=="logistic"){
result<-(-1)*(tanh((sqrt(u))/2))/(2*sqrt(u))
}
return(result)
}

