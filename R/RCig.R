`RCig` <-
function(x,cases.removed=NULL,kernel="normal"){
data<-x
ker<-kernel
estimacion<-mleig(data,ker)
mu<-estimacion$mu
lambda<-estimacion$lambda
eliminados<-data[cases.removed]
newdata<-data[-as.vector(cases.removed)]
estimacion2<-mleig(newdata,ker)
mui<-estimacion2$mu
lambdai<-estimacion2$lambda
RCmu<-abs(((mu-mui)/mu))*100
RCmu<-round(RCmu,2)
RClambda<-abs(((lambda-lambdai)/lambda))*100
RClambda<-round(RClambda,2)
lista<-list(Cases.removed=eliminados,RelativeChanges.mu=RCmu,RelativeChanges.lambda=RClambda)
return(lista)
}

