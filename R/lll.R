`lll` <-
function(theta,x,kernel="normal",nu=1.0){
ker<-kernel
mu<-theta[1]
lambda<-theta[2]
nu<-nu
argu<-kappaII(x,c(mu,lambda))
vi<-(-2)*(wg(argu,ker,nu))
vip<-(-2)*(wgp(argu,ker,nu))
n<-length(x)
a<-((x/(mu^2))+(1/x)-(2/mu))^2
b<-vip*a
c<-sum(b)
d<-(-n/(2*(lambda^2)))-((1/2)*c)
return(d)
}

