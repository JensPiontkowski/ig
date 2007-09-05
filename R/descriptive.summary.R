`descriptive.summary` <-
function(x){
library(fBasics)
data<-x
prom<-mean(data)
mediana<-median(data)
a<-search.mode(data)
moda<-a$mode
desv<-sd(data)
rango<-(max(data)-min(data))
CV<-(sd(data)/mean(data))*100
CS<-skewness(data)
CK<-kurtosis(data)
mini<-min(data)
maxi<-max(data)
ene<-length(data)
lista<-list(Mean=prom,Median=mediana,Mode=moda,SD=desv,CV=CV,CS=CS,CK=CK,Range=rango,Min=mini,Max=maxi,n=ene)
return(lista)
}

