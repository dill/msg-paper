# function to do the smoothing on the Ramsay horseshoe 
# Copyright David Lawrence Miller 2009
library(soap)

samp.size=250
noise.level=0.5
## create a boundary...
bnd <- fs.boundary()
# create points within the boundary 
m<-100;n<-100
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]


# make the sample
samp.ind<-sample(1:length(xx),samp.size)

# add noise
noise<-rnorm(samp.size)*noise.level

samp.data<-data.frame(x=xx[samp.ind],y=yy[samp.ind],
                      z=fs.test(xx[samp.ind],yy[samp.ind])+noise)


# prediction data for non mds'd
pred.data<-list(x=xx,y=yy,z=fs.test(xx,yy))

# boundary, only for drawing the line around the outside
fsb <- fs.boundary()

# truth
z.truth<-matrix(NA,m,n)
z.truth[onoff]<-fs.test(xx,yy)
   
### normal tprs
b.tprs<-gam(z~s(x,y,k=100),data=samp.data)
fv.tprs <- predict(b.tprs,newdata=pred.data)



#par(mfrow=c(1,2))
par(mfrow=c(2,1))
par(mar=c(0.2,1,0.2,1))

# truth
image(xm,yn,z.truth,col=heat.colors(100),xlab="",ylab="",main="",las=1,asp=1,lwd=2, axes=FALSE)
contour(xm,yn,z.truth,levels=seq(-5,5,by=.25),add=TRUE,labcex=0.3,lwd=0.5)
lines(fsb,lwd=2)

# tprs
pred.mat<-matrix(NA,m,n)
pred.mat[onoff]<-fv.tprs
image(xm,yn,pred.mat,col=heat.colors(100),xlab="x",ylab="y",main="",las=1,asp=1,lwd=2,axes=FALSE)
contour(xm,yn,pred.mat,levels=seq(-5,5,by=.25),add=TRUE,labcex=0.3,lwd=0.5)
lines(fsb,lwd=2)

#dev.copy2pdf(file="ramsay-leak.pdf",width=5,height=1.25)
#dev.copy2eps(file="ramsay-leak.ps",width=5,height=1.25)
dev.copy2pdf(file="ramsay-leak.pdf",width=2.5,height=3)
dev.copy2eps(file="ramsay-leak.ps",width=2.5,height=3)
