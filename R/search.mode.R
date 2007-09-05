`search.mode` <-
function(x){
datos<-x
unico<-unique(datos)
n<-length(unico)
ene<-length(datos)
frecuencias<-seq(1,n,by=1)
suma<-0
for(i in 1:n){
suma<-0
for(j in 1:ene){
if(unico[i]==datos[j]){
suma<-(suma+1)
}
} 
frecuencias[i]<-suma
} 
maximo<-max(frecuencias)
indi.max<-which(frecuencias==maximo)
enne<-length(indi.max)
modas<-seq(1,enne,by=1)
for(k in 1:enne){
indice<-indi.max[k]
moda<-unico[indice]
modas[k]<-moda
}
result<-modas
resultados<-list(number.of.results=enne,mode=modas,frecuency=maximo)
return(resultados)
}

