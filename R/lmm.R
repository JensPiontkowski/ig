`lmm` <-
function(theta,x,kernel="normal",nu=1.0){
ker<-kernel
mu<-theta[1]
lambda<-theta[2]
nu<-nu
argu<-kappaII(x,c(mu,lambda))
vi<-(-2)*(wg(argu,ker,nu))
vip<-(-2)*(wgp(argu,ker,nu))
n<-length(x)
a<-(lambda*((3*x)-(2*mu)))/(mu^4)
b<-vi*a
c<-sum(b)
d<-((lambda^2)*((x-mu)^2))/(mu^6)
e<-vip*d
f<-sum(e)
g<-(((-2)*f)-c)
return(g)
}

