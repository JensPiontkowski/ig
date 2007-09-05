`diagnosticsig` <-
function(data,kernel="normal",
main="",ylim=NULL){
ylim<-ylim
ker<-kernel
data<-data
if(kernel=="normal"){
estimas<-mleig(data,kernel="normal")
estimacion<-c(estimas$mu,estimas$lambda)
npar<-2
ene<-length(data)
hessian<-matrix(NA,2,2)
hessian[1,1]<-lmm(estimacion,data,"normal")
hessian[1,2]<-lml(estimacion,data,"normal")
hessian[2,1]<-lml(estimacion,data,"normal")
hessian[2,2]<-lll(estimacion,data,"normal")
varcov<- -solve(hessian)
#local influence
kernel<-"normal"
nu<-2.0
n<-length(data)
ker<-kernel
nu<-nu
Dm<-vector("numeric")
Dl<-vector("numeric")
estimas<-mleig(data,ker)
nu<-estimas$nu
estimacion<-c(estimas$mu,estimas$lambda)
argu<-kappaII(data,estimacion)
vi<-(-2)*(wg(argu,ker,nu))
mu<-estimacion[1]
lambda<-estimacion[2]
x<-data
Dm<-vi*((lambda*(x-mu))/(mu^3))
Dl<-(1/(2*lambda))-(1/2)*vi*((x/(mu^2))+(1/x)-(2/mu))
Delta <- rbind(Dm,Dl)
B<-t(Delta)%*%solve(hessian)%*%Delta
EB<-eigen(B)
eigenvalues<-eigen(B)$values
eigenvectors<-eigen(B)$vectors
#Ci#
l<-vector("numeric")
for(i in 1:n)
{
l[i] <- 2*abs(B[i,i])
}
ci<-2*mean(l)
plot(l,type="h",main="",ylim=ylim,
xlab="Index", ylab=expression(C[i]),cex.main=1.5,las=1,col=4,lwd=1.5) 
lines(c(-10,n+10),c(ci,ci),col=2,lwd=2.0)
} # Fin "normal"
if(kernel=="t"){
estimas<-mleig(data,kernel="t")
estimacion<-c(estimas$mu,estimas$lambda)
gl<-estimas$nu
npar<-2
ene<-length(data)
hessian<-matrix(NA,2,2)
hessian[1,1]<-lmm(estimacion,data,"t",gl)
hessian[1,2]<-lml(estimacion,data,"t",gl)
hessian[2,1]<-lml(estimacion,data,"t",gl)
hessian[2,2]<-lll(estimacion,data,"t",gl)
varcov<- -solve(hessian)
kernel<-"t"
nu<-2.0
n<-length(data)
ker<-kernel
nu<-nu
Dm<-vector("numeric")
Dl<-vector("numeric")
estimas<-mleig(data,ker)
nu<-estimas$nu
estimacion<-c(estimas$mu,estimas$lambda)
argu<-kappaII(data,estimacion)
vi<-(-2)*(wg(argu,ker,nu))
mu<-estimacion[1]
lambda<-estimacion[2]
x<-data
Dm<-vi*((lambda*(x-mu))/(mu^3))
Dl<-(1/(2*lambda))-(1/2)*vi*((x/(mu^2))+(1/x)-(2/mu))
Delta <- rbind(Dm,Dl)
B<-t(Delta)%*%solve(hessian)%*%Delta
EB<-eigen(B)
eigenvalues<-eigen(B)$values
eigenvectors<-eigen(B)$vectors
#Ci#
l<-vector("numeric")
for(i in 1:n)
{
l[i] <- 2*abs(B[i,i])
}
ci<-2*mean(l)
plot(l,type="h",main="",ylim=ylim, 
xlab="Index", ylab=expression(C[i]),cex.main=1.5,las=1,col=4,lwd=1.5)
lines(c(-10,n+10),c(ci,ci),col=2,lwd=2.0)
} # Fin "t-Student"
if(kernel=="Laplace"){
estimas<-mleig(data,kernel="Laplace")
estimacion<-c(estimas$mu,estimas$lambda)
npar<-2
ene<-length(data)
hessian<-matrix(NA,2,2)
hessian[1,1]<-lmm(estimacion,data,"Laplace")
hessian[1,2]<-lml(estimacion,data,"Laplace")
hessian[2,1]<-lml(estimacion,data,"Laplace")
hessian[2,2]<-lll(estimacion,data,"Laplace")
varcov<- -solve(hessian)
kernel<-"Laplace"
nu<-2.0
n<-length(data)
ker<-kernel
nu<-nu
Dm<-vector("numeric")
Dl<-vector("numeric")
estimas<-mleig(data,ker)
nu<-estimas$nu
estimacion<-c(estimas$mu,estimas$lambda)
argu<-kappaII(data,estimacion)
vi<-(-2)*(wg(argu,ker,nu))
mu<-estimacion[1]
lambda<-estimacion[2]
x<-data
Dm<-vi*((lambda*(x-mu))/(mu^3))
Dl<-(1/(2*lambda))-(1/2)*vi*((x/(mu^2))+(1/x)-(2/mu))
Delta <- rbind(Dm,Dl)
B<-t(Delta)%*%solve(hessian)%*%Delta
EB<-eigen(B)
eigenvalues<-eigen(B)$values
eigenvectors<-eigen(B)$vectors
#Ci#
l<-vector("numeric")
for(i in 1:n)
{
l[i] <- 2*abs(B[i,i])
}
ci<-2*mean(l)
plot(l,type="h",main="",ylim=ylim, 
xlab="Index", ylab=expression(C[i]),cex.main=1.5,las=1,col=4,lwd=1.5)
lines(c(-10,n+10),c(ci,ci),col=2,lwd=2.0)
} # Fin "Laplace"
if(kernel=="logistic"){
estimas<-mleig(data,kernel="logistic")
estimacion<-c(estimas$mu,estimas$lambda)
npar<-2
ene<-length(data)
hessian<-matrix(NA,2,2)
hessian[1,1]<-lmm(estimacion,data,"logistic")
hessian[1,2]<-lml(estimacion,data,"logistic")
hessian[2,1]<-lml(estimacion,data,"logistic")
hessian[2,2]<-lll(estimacion,data,"logistic")
varcov<- -solve(hessian)
kernel<-"logistic"
nu<-2.0
n<-length(data)
ker<-kernel
nu<-nu
Dm<-vector("numeric")
Dl<-vector("numeric")
estimas<-mleig(data,ker)
nu<-estimas$nu
estimacion<-c(estimas$mu,estimas$lambda)
argu<-kappaII(data,estimacion)
vi<-(-2)*(wg(argu,ker,nu))
mu<-estimacion[1]
lambda<-estimacion[2]
x<-data
Dm<-vi*((lambda*(x-mu))/(mu^3))
Dl<-(1/(2*lambda))-(1/2)*vi*((x/(mu^2))+(1/x)-(2/mu))
Delta <- rbind(Dm,Dl)
B<-t(Delta)%*%solve(hessian)%*%Delta
EB<-eigen(B)
eigenvalues<-eigen(B)$values
eigenvectors<-eigen(B)$vectors
#Ci#
l<-vector("numeric")
for(i in 1:n)
{
l[i] <- 2*abs(B[i,i])
}
ci<-2*mean(l)
plot(l,type="h",main="",ylim=ylim, 
xlab="Index", ylab=expression(C[i]),las=1,cex.main=1.5,las=1,col=4,lwd=1.5)
lines(c(-10,n+10),c(ci,ci),col=2,lwd=2.0)
} # Fin "logistic"
}

