`mleIGt` <-
function(x,nu.fixed=2.0){
inicials<-mleIG(x)
mu.start<-inicials$mu
lambda.start<-inicials$lambda
nu.start<-nu.fixed
data<-x
dgigt<-function(x,mu=1.0,lambda=1.0,nu=1.0,log=FALSE){
cantidad<-sqrt(lambda/mu)*(sqrt(x/mu)-sqrt(mu/x))
jacobiano<-sqrt(lambda)/sqrt(x^3)
density<-dt(cantidad,nu)*jacobiano
if(log==TRUE){density<-log(density)}
return(density)
}
Loglik<-function(theta,x){
sum(-dgigt(x,mu=theta[1],lambda=theta[2],nu=nu.fixed,log=TRUE))
}
theta.start<-c(mu.start,lambda.start,nu.start)
W<-nlm(Loglik,theta.start,x=data)
estimates<-W$estimate
esti<-list(mu=estimates[1],lambda=estimates[2],nu=estimates[3])
return(esti)
}

