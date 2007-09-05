`lml` <-
function(theta,x,kernel="normal",nu=1.0){
ker<-kernel
mu<-theta[1]
lambda<-theta[2]
nu<-nu
argu<-kappaII(x,c(mu,lambda))
vi<-(-2)*(wg(argu,ker,nu))
vip<-(-2)*(wgp(argu,ker,nu))
n<-length(x)
a<-((x-mu)/(mu^3))
b<-vi*a
c<-sum(b)
d<-((lambda*(x-mu))/(mu^3))*((x/(mu^2))+(1/x)-(2/mu))
e<-vip*d
f<-sum(e)
g<-(f+c)
return(g)
}

