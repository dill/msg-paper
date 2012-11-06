## plot some fitted functions

library(mgcv)
library(msg)

dat2<-read.csv(file="ramsay-ex.csv")
dat2<-as.matrix(dat2[,-1])

## Initialisation
# make the horseshoe boundary
bnd <- fs.boundary()
# simplify the boundary
bnd<-as.data.frame(bnd)[seq(1,length(bnd$x),8),]
fs.bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))

# create the prediction grid and the points to sample
# from
m<-100;n<-100
xm <- seq(-1,3.5,length=m); yn<-seq(-1,1,length=n)
xx <- rep(xm,n); yy<-rep(yn,rep(m,n))
onoff <- inSide(fs.bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]
fs.data<-data.frame(x=xx,y=yy,z=fs.test(xx,yy))
pred.data<-data.frame(x=xx,y=yy)

# old, crappy way of doing this
im<-matrix(NA,m,n)

par(mfrow=c(2,3),mar=c(1,1,1,1),mgp=c(0.75,0.75,0),oma=c(1,1,1,1))

# truth
im[onoff]<-dat2[4,]
image(z=im,x=xm,y=yn,col=heat.colors(100),main="truth",asp=1,xlab="",ylab="",zlim=c(-5,5),las=1, lwd=2,axes=FALSE)
contour(z=im,x=xm,y=yn,levels=seq(-5,5,by=.25),add=TRUE,labcex=0.3,lwd=0.5)
lines(fs.boundary(),lwd=2)


# tprs
im[onoff]<-dat2[1,]
image(z=im,x=xm,y=yn,col=heat.colors(100),main="tprs",asp=1,xlab="",ylab="",zlim=c(-5,5),las=1, lwd=2,axes=FALSE)
contour(z=im,x=xm,y=yn,levels=seq(-5,5,by=.25),add=TRUE,labcex=0.3,lwd=0.5)
lines(fs.boundary(),lwd=2)

# mds
im[onoff]<-dat2[3,]
image(z=im,x=xm,y=yn,col=heat.colors(1000),main="mdsds",asp=1,xlab="",ylab="",zlim=c(-5,5),las=1,axes=FALSE)
contour(z=im,x=xm,y=yn,levels=seq(-5,5,by=.25),add=TRUE,labcex=0.3,lwd=0.5)
lines(fs.boundary(),lwd=2)


# soap
im[onoff]<-dat2[2,]
image(z=im,x=xm,y=yn,col=heat.colors(1000),main="soap",asp=1,xlab="",ylab="",zlim=c(-5,5),las=1,axes=FALSE)
contour(z=im,x=xm,y=yn,levels=seq(-5,5,by=.25),add=TRUE,labcex=0.3,lwd=0.5)
lines(fs.boundary(),lwd=2)

# soap
im[onoff]<-dat2[5,]
image(z=im,x=xm,y=yn,col=heat.colors(1000),main="gltps",asp=1,xlab="",ylab="",zlim=c(-5,5),las=1,axes=FALSE)
contour(z=im,x=xm,y=yn,levels=seq(-5,5,by=.25),add=TRUE,labcex=0.3,lwd=0.5)
lines(fs.boundary(),lwd=2)

#dev.copy2eps(file="ramsay-real.eps",width=2,height=2.2*1.8)
#dev.copy2pdf(file="ramsay-real.pdf",width=2,height=2.2*1.8)

#dev.copy2pdf(file="ramsay-real.pdf",width=3*1.8,height=2*1.1)
dev.copy2eps(file="Fig1.eps",width=3*1.8,height=2*1.1)

