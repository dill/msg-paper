# make a boxplot of the ramsay results

library(ggplot2)

source("pe.R")


dat<-read.csv("ramsay-results.csv")
dat<-dat[,-1]
names(dat)<-c("model","noise","n","i","mse","mdsdim")

dat<-dat[dat$n==600,]
dat$model<-as.character(dat$model)
dat$model[dat$model=="wr"]<-"gltps"
dat$model<-as.factor(dat$model)

theme_set(theme_bw())
p<-ggplot(dat)
p<-p+geom_boxplot(aes(x=model,y=log(mse)))
#p<-p+facet_grid(noise~n)
p<-p+facet_wrap(~noise,nrow=1)
p<-p+labs(x="Model",y="log(mean MSE) per realisation")
p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          panel.background=theme_rect())
print(p)

ggsave(file="ramsay-result.eps",height=3.5,width=6)
ggsave(file="ramsay-result.pdf",height=3.5,width=6)
dev.off()

# plot the MDS projection dimension
#dat<-dat[dat$model=="mdsds",]
#
#p<-ggplot(dat)
#p<-p+geom_histogram(aes(mdsdim))
#p<-p+facet_grid(noise~n)
#p<-p+opts(panel.grid.major=theme_blank(),
#          panel.grid.minor=theme_blank(),
#          panel.background=theme_rect())
#p<-p+labs(x="MDS dimension projection",y="Count")
#
#print(p)


## plot some fitted functions

library(mgcv)
library(soap)
library(msg)

dat2<-read.csv(file="ramsay-ex.csv")
dat2<-as.matrix(dat2[,-1])

## Initialisation
# make the horseshoe boundary
bnd <- fs.boundary()
bnd<-pe(bnd,seq(1,length(bnd$x),8)) # simplify the boundary
fs.bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))

# create the prediction grid and the points to sample
# from
m<-100;n<-100
xm <- seq(-1,3.5,length=m); yn<-seq(-1,1,length=n)
xx <- rep(xm,n); yy<-rep(yn,rep(m,n))
onoff<-soap:::inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]
fs.data<-data.frame(x=xx,y=yy,z=fs.test(xx,yy))
pred.data<-data.frame(x=xx,y=yy)

# old, crappy way of doing this
im<-matrix(NA,m,n)

par(mfrow=c(5,1),mar=c(1,1.5,1,1.5),mgp=c(1.5,0.75,0),oma=c(1,1,1,1))

# truth
im[onoff]<-dat2[4,]
image(z=im,x=xm,y=yn,col=heat.colors(1000),main="truth",asp=1,xlab="",ylab="",zlim=c(-5,5),las=1,axes=FALSE)
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

# wr
im[onoff]<-dat2[5,]
image(z=im,x=xm,y=yn,col=heat.colors(1000),main="gltps",asp=1,xlab="",ylab="",zlim=c(-5,5),las=1,axes=FALSE)
contour(z=im,x=xm,y=yn,levels=seq(-5,5,by=.25),add=TRUE,labcex=0.3,lwd=0.5)
lines(fs.boundary(),lwd=2)


dev.copy2eps(file="ramsay-real.eps",width=2,height=5)
dev.copy2pdf(file="ramsay-real.pdf",width=2,height=5)


# (paired) Wilcoxon signed rank test
tprs.dat<-dat[dat$model=="tprs",]
soap.dat<-dat[dat$model=="soap",]
mdsds.dat<-dat[dat$model=="mdsds",]
gltps.dat<-dat[dat$model=="gltps",]

for(n in unique(tprs.dat$n)){
   for(noise in unique(tprs.dat$noise)){

      this.tprs<-tprs.dat[tprs.dat$n==n & tprs.dat$noise==noise,]
      this.gltps<-gltps.dat[gltps.dat$n==n & gltps.dat$noise==noise,]
      this.soap<-soap.dat[soap.dat$n==n & soap.dat$noise==noise,]
      this.mdsds<-mdsds.dat[mdsds.dat$n==n & mdsds.dat$noise==noise,]

      p<-wilcox.test(this.tprs$mse,this.mdsds$mse,paired=TRUE)$p.value
      cat("tprs noise=",noise, 
        sign(median(this.mdsds$mse)-median(this.tprs$mse)),"p=",p,"\n")

      p<-wilcox.test(this.soap$mse,this.mdsds$mse,paired=TRUE)$p.value
      cat("soap noise=",noise,
        sign(median(this.mdsds$mse)-median(this.soap$mse)),"p=",p,"\n")

      p<-wilcox.test(this.gltps$mse,this.mdsds$mse,paired=TRUE)$p.value
      cat("gltps noise=",noise,
        sign(median(this.mdsds$mse)-median(this.gltps$mse)),"p=",p,"\n")

   }
}



#median(mdsds.dat$mdsdim)
