`kappaII` <-
function(t=1.0,theta=c(1,1)){
t<-t;theta<-theta
m<-theta[1];l<-theta[2]
value<-((l/m)*((t/m)+(m/t)-2))
return(value)
}

