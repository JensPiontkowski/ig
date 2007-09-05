`dig.PVII` <-
function(t,mu,lambda,parameters=c(1.0,1.0),log=FALSE){
dPVII<-function(x,q=1.0,r=1.0){
c<-(gamma(q))/(sqrt(r*pi)*gamma(q-(1/2)))
g<-(1+((x^2)/r))^(-q)
dsty<-c*g
return(dsty)
}
cantidad<-sqrt(lambda/mu)*(sqrt(t/mu)-sqrt(mu/t))
jacobiano<-sqrt(lambda)/sqrt((t^3))
q<-parameters[1]
r<-parameters[2]
pdf<-dPVII(cantidad,q,r)*jacobiano
if(log==TRUE){pdf<-log(pdf)}
return(pdf)
}

