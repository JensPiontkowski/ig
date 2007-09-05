`rig` <-
function(n,mu=1.0,lambda=1.0,kernel="normal",parameter.nu=1.0){
if (n==0) stop("Value of n must be greater or equal then 0")
if(mu<=0) stop("mu must be positive")
if(lambda<=0) stop("lambda must be positive")
if(kernel=="normal"){
v0<-rchisq(n,1)
}
if(kernel=="t"){
nu<-parameter.nu
v0<-rf(n,1,nu)
}
if(kernel=="logistic"){
v0<-rbeta(n,1,1)
}
if(kernel=="Laplace"){
nu<-1/2
v0<-rgamma(n,1/2,1/nu)
}
x1<-mu+(((mu^2)*v0)/(2*lambda))-(mu/(2*lambda))*sqrt(4*mu*lambda*v0+((mu^2)*(v0^2)))
x2<-(mu^2)/x1
p0<-mu/(mu+x1)
u0<-runif(n,0,1)
Y<-seq(1,n,by=1)
for(i in 1:n){
val1<-u0[i]
val2<-p0[i]
if (val1<=val2){Y[i]<-x1[i]} else {Y[i]<-x2[i]}
}
sample<-Y
return(sample)
}

