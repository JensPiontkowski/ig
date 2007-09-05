`dig.kotz` <-
function(t,mu,lambda,parameters=c(1.0,1.0,1.0),log=FALSE){
dkotz<-function(x,theta=c(1.0,1.0,1.0)){
q<-theta[1]
r<-theta[2]
s<-theta[3]
argum<-((2*q)-1)/(2*s)
c<-(s*(r^argum))/gamma(argum)
g<-(x^(2*(q-1)))*exp(-r*(x^(2*s)))
dsty<-c*g
return(dsty)
}
cantidad<-sqrt(lambda/mu)*(sqrt(t/mu)-sqrt(mu/t))
jacobiano<-sqrt(lambda)/sqrt((t^3))
q<-parameters[1]
r<-parameters[2]
s<-parameters[3]
pdf<-dkotz(cantidad,c(q,r,s))*jacobiano
if(log==TRUE){pdf<-log(pdf)}
return(pdf)
}

