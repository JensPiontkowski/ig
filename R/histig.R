`histig` <-
function(x,main="Histogram and boxplot",xlab="Data",ylab="Frequency",pdf="FALSE",kernel.pdf="normal",
col=NULL,boxplot="TRUE",col.pdf=1,col.boxplot=4){
nf <- layout(matrix(c(1,1,2,2),2,2,byrow=TRUE), c(1,7), c(7,1), TRUE)
layout.show(nf)
par(mar=c(7,5,2,5))
vals<-hist(x,freq=F,plot=FALSE)
mini<-min(x)-sd(x)
maxi<-max(x)+sd(x)
eje<-seq(mini,maxi,by=0.1)
ker<-kernel.pdf
estimates<-mleig(x,kernel=ker)
m<-estimates$mu
la<-estimates$lambda
nu<-estimates$nu
y<-dig(eje,mu=m,lambda=la,kernel=ker,nu)
scaling<-max(y)-max(vals$density)
maxlim<-max(vals$density)+scaling
hist(x,freq=F,ylim=c(0,maxlim),main = main, xlab = xlab, 
ylab = ylab,cex.main=2.0,col=col,mgp=c(4,1,0),las=1)
rug(x, side=1)
if(pdf=="TRUE"){
lines(eje,y,lwd=2.0,col=col.pdf)
}
par(mar=c(0,5,0,5))
if(boxplot=="TRUE"){
boxplot.default(x,axes=FALSE,horizontal = TRUE,notch=TRUE,col=col.boxplot)
}
}

