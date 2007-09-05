`ACIig` <-
function(x,kernel="normal",conf.level=95,chart=c(NULL,NULL,NULL,NULL),col=1){
data<-x
uno<-chart[1]
dos<-chart[2]
tres<-chart[3]
cuatro<-chart[4]
alpha<-1-(conf.level/100)
crit<-round(qchisq(1-alpha,2),2) # chi-square percentil
# ellipse
elipse<-function(A,m,const,k){
r <- A[1, 2]/sqrt(A[1, 1] * A[2, 2])
Q <- matrix(0, 2, 2) # This builds a null matrix Q
Q[1, 1] <- sqrt(A[1, 1] %*% (1+r)/2) # This transforms the unit circle
Q[1, 2] <- -sqrt(A[1, 1] %*% (1-r)/2)# in a ellipse
Q[2, 1] <- sqrt(A[2, 2] %*% (1+r)/2)
Q[2, 2] <- sqrt(A[2, 2] %*% (1-r)/2)
alpha <- seq(0, by = (2 * pi)/k, length = k) # This define angles for ploting
Z <- cbind(cos(alpha), sin(alpha)) # This define coordinates of points on a unit circle
X <- t(m + const * Q %*% t(Z)) # This define coordinates of points on the ellipse
return(X)
}
# Points on the ellipse
puntos<-1000
if(kernel=="normal"){
estimas<-mleig(data,kernel="normal")
estimacion<-c(estimas$mu,estimas$lambda)
mu<-round(estimacion[1],2)
lambda<-round(estimacion[2],2)
hessian<-matrix(NA,2,2)
hessian[1,1]<-lmm(estimacion,data,"normal")
hessian[1,2]<-lml(estimacion,data,"normal")
hessian[2,1]<-lml(estimacion,data,"normal")
hessian[2,2]<-lll(estimacion,data,"normal")
varcov<- -solve(hessian)
varianza<-varcov
c<-sqrt(crit)
m<-estimacion
X<-elipse(A=varianza,m=estimacion,const=c,k=puntos)
plot(X[,1],X[,2],type="l",xlab=expression(mu),ylab=expression(lambda),xlim=c(uno,dos),ylim=c(tres,cuatro),lwd=2,col=col)
points(mu,lambda,pch=20,col=2,lwd=2)
A<-X
interl<-c(round(min(A[,2]),2),round(max(A[,2]),2))
intervalol<-sort(interl)
interm<-c(round(min(A[,1]),2),round(max(A[,1]),2))
intervalom<-sort(interm)
res<-list(estimate.mu=mu,ACIig.mu=intervalom,estimate.lambda=lambda,ACIig.lambda=intervalol)
}
if(kernel=="t"){
estimas<-mleig(data,kernel="t")
estimacion<-c(estimas$mu,estimas$lambda)
gl<-estimas$nu
mu<-round(estimacion[1],2)
lambda<-round(estimacion[2],2)
hessian<-matrix(NA,2,2)
hessian[1,1]<-lmm(estimacion,data,"t",gl)
hessian[1,2]<-lml(estimacion,data,"t",gl)
hessian[2,1]<-lml(estimacion,data,"t",gl)
hessian[2,2]<-lll(estimacion,data,"t",gl)
varcov<- -solve(hessian)
varianza<-varcov
c<-sqrt(crit)
m<-estimacion
X<-elipse(A=varianza,m=estimacion,const=c,k=puntos)
plot(X[,1],X[,2],type="l",xlab=expression(mu),ylab=expression(lambda),xlim=c(uno,dos),ylim=c(tres,cuatro),lwd=2,col=col)
points(mu,lambda,pch=20,col=2,lwd=2)
A<-X
interl<-c(round(min(A[,2]),2),round(max(A[,2]),2))
intervalol<-sort(interl)
interm<-c(round(min(A[,1]),2),round(max(A[,1]),2))
intervalom<-sort(interm)
res<-list(estimate.mu=mu,ACIig.mu=intervalom,estimate.lambda=lambda,ACIig.lambda=intervalol)
}
if(kernel=="Laplace"){
estimas<-mleig(data,kernel="Laplace")
estimacion<-c(estimas$mu,estimas$lambda)
mu<-round(estimacion[1],2)
lambda<-round(estimacion[2],2)
hessian<-matrix(NA,2,2)
hessian[1,1]<-lmm(estimacion,data,"Laplace")
hessian[1,2]<-lml(estimacion,data,"Laplace")
hessian[2,1]<-lml(estimacion,data,"Laplace")
hessian[2,2]<-lll(estimacion,data,"Laplace")
varcov<- -solve(hessian)
varianza<-varcov
c<-sqrt(crit)
m<-estimacion
X<-elipse(A=varianza,m=estimacion,const=c,k=puntos)
plot(X[,1],X[,2],type="l",xlab=expression(mu),ylab=expression(lambda),xlim=c(uno,dos),ylim=c(tres,cuatro),lwd=2,col=col)
points(mu,lambda,pch=20,col=2,lwd=2)
A<-X
interl<-c(round(min(A[,2]),2),round(max(A[,2]),2))
intervalol<-sort(interl)
interm<-c(round(min(A[,1]),2),round(max(A[,1]),2))
intervalom<-sort(interm)
res<-list(estimate.mu=mu,ACIig.mu=intervalom,estimate.lambda=lambda,ACIig.lambda=intervalol)
}
if(kernel=="logistic"){
estimas<-mleig(data,kernel="logistic")
estimacion<-c(estimas$mu,estimas$lambda)
mu<-round(estimacion[1],2)
lambda<-round(estimacion[2],2)
hessian<-matrix(NA,2,2)
hessian[1,1]<-lmm(estimacion,data,"logistic")
hessian[1,2]<-lml(estimacion,data,"logistic")
hessian[2,1]<-lml(estimacion,data,"logistic")
hessian[2,2]<-lll(estimacion,data,"logistic")
varcov<- -solve(hessian)
varianza<-varcov
c<-sqrt(crit)
m<-estimacion
X<-elipse(A=varianza,m=estimacion,const=c,k=puntos)
plot(X[,1],X[,2],type="l",xlab=expression(mu),ylab=expression(lambda),xlim=c(uno,dos),ylim=c(tres,cuatro),lwd=2,col=col)
points(mu,lambda,pch=20,col=2,lwd=2)
A<-X
interl<-c(round(min(A[,2]),2),round(max(A[,2]),2))
intervalol<-sort(interl)
interm<-c(round(min(A[,1]),2),round(max(A[,1]),2))
intervalom<-sort(interm)
res<-list(estimate.mu=mu,ACIig.mu=intervalom,estimate.lambda=lambda,ACIig.lambda=intervalol)
}
return(res)
}

