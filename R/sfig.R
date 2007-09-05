`sfig` <-
function(t,mu=1.0,lambda=1.0,kernel="normal",parameter.nu=1.0){
m<-mu
l<-lambda
nu<-parameter.nu
ker=kernel
rel<-1-pig(t,m,l,ker,nu)
return(rel)
}

