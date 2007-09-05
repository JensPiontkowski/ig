`lmu` <-
function(theta,x,kernel="normal",nu=1.0){
ker<-kernel
mu<-theta[1]
lambda<-theta[2]
nu<-nu
argu<-kappaII(x,c(mu,lambda))
vi<-(-2)*(wg(argu,ker,nu))
a<-(lambda*(x-mu))/(mu^3)
b<-vi*a
c<-sum(b)
return(c)
}

