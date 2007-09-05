`mleIG` <-
function(x){
n<-length(x)
prom<-(sum(x))/n
mu<-prom
a<-(1/x)
b<-(1/prom)
lambda<-(n/(sum(a-b)))
lista<-list(mu=mu,lambda=lambda)
return(lista)
}

