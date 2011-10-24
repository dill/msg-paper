# Aral Sea data set analysis

# libraries
library(mgcv)
library(soap)
library(msg)

source("latlong2km.R")

# load the data and boundary
aral<-read.csv("aral.dat",sep=" ")
bnd<-read.csv("aralbnd.csv")

#zlims<-c(1.905461, 19.275249)
zlims<-c(1, 17)
z.levels<-pretty(zlims,15)

# first cut out the crap using inSide
onoff<-inSide(bnd,aral$lo,aral$la)

# converstion to km
aral.km<-latlong2km(aral$lo[onoff],aral$la[onoff],59.5,45)

aral.dat<-data.frame(x=aral.km$km.e,
                     y=aral.km$km.n,
                     chl=as.numeric(aral$chl[onoff]))

names(aral.dat)<-c("x","y","z")

# convert boundary to northings and eastings
bnd.km<-latlong2km(bnd[,2],bnd[,3],59.5,45)
bnd<-list(x=bnd.km$km.e,y=bnd.km$km.n)


# prediction grid
gm<-50;gn<-50
gxm <- seq(min(aral.dat$x),max(aral.dat$x),length=gm)
gyn<-seq(min(aral.dat$y),max(aral.dat$y),length=gn)
gxx <- rep(gxm,gn)
gyy<-rep(gyn,rep(gm,gn))
pred.onoff<-inSide(bnd,gxx,gyy)
pred.grid<-data.frame(x=gxx[pred.onoff],y=gyy[pred.onoff])

######################################################################
# plot setup
par(mfrow=c(2,2),las=1,mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)

# set the x and y values for the image plot
aral.lab<-latlong2km(unique(sort(aral$lo)),unique(sort(aral$la)),59.5,45)
# set the plot limits
xlims<-c(min(aral.dat$x)-10,max(aral.dat$x)+10)
ylims<-c(min(aral.dat$y)-10,max(aral.dat$y)+10)

######################################################################
#### plot some raw data

aral$chl[!onoff]<-NA
image(z=matrix(aral$chl,46,46),x=aral.lab$km.e,y=aral.lab$km.n,
      asp=1,main="raw data",xlab="km (East)",ylab="km (North)",xlim=xlims,ylim=ylims,zlim=zlims)
lines(bnd,lwd=2)

#######################################################################
##### fit a thin plate model
tp.fit<-gam(z~s(x,y,k=70),data=aral.dat,family=Gamma(link="log"))

tp.pred<-predict(tp.fit,newdata=pred.grid,type="response")
pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-tp.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),main="tprs",xlab="km (East)",ylab="km (North)",xlim=xlims,ylim=ylims,asp=1,zlim=zlims)
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,levels=z.levels)
lines(bnd,lwd=2)

######################################################################
#### soap 
s.knots<-make_soap_grid(bnd,c(12,12))

soap.fit<-gam(z~s(x,y,k=49,bs="so",xt=list(bnd=list(bnd))),knots=s.knots,
            family=Gamma(link="log"),data=aral.dat)

# prediction
soap.pred<-predict(soap.fit,newdata=pred.grid,type="response")
pred.mat<-matrix(NA,gm,gn)
pred.mat[pred.onoff]<-soap.pred
image(pred.mat,x=unique(gxx),y=unique(gyy),xlab="km (East)",ylab="km (North)",main="soap",xlim=xlims,ylim=ylims,asp=1,zlim=zlims)
contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,levels=z.levels)
lines(bnd,lwd=2)


######################################################################
#### MDS

names(aral.dat)<-c("x","y","z")

plot.it<-function(dat,main.title){
   zlims<-c(1, 20)
   pred.mat<-matrix(NA,gm,gn)
   pred.mat[pred.onoff]<-dat$pred
   image(pred.mat,x=unique(gxx),y=unique(gyy),main=main.title,
         xlab="km (East)",ylab="km (North)",xlim=xlims,ylim=ylims,asp=1,zlim=zlims)
   contour(z=pred.mat,x=unique(gxx),y=unique(gyy),add=TRUE,labcex=0.5,zlim=zlims,levels=z.levels)
   lines(bnd,lwd=2)
}

mds.fit<-gam.mds(aral.dat,pred.grid,bnd,grid.res=c(20,20),family=Gamma(link="log"),gam.method="GCV.Cp") 
plot.it(mds.fit,"mdsds")

#mds2<-gam.mds(aral.dat,pred.grid,bnd,grid.res=c(20,20))
#plot.it(mds2,"mds 2D - tprs")

# reset the mds dimension, so we can re-use Ds...
#mds2$mds.dim<-NULL
#mds2$m<-NULL
#mds2$bs<-NULL
#
#mds3<-gam.mds(aral.dat,pred.grid,bnd,grid.res=c(20,20),mds.dim=3,old.obj=mds2,family=Gamma(link="log")) 
#plot.it(mds3,"mds 3D - tprs")
#
#mds3.ds<-gam.mds(aral.dat,pred.grid,bnd,grid.res=c(20,20),mds.dim=3,m=c(2,3/2-1),bs="ds",old.obj=mds2,family=Gamma(link="log"))
#plot.it(mds3.ds,"mds 3D - ds, s=0.5")




### how does GCV vary with k
#res<-c()
#
#for(expl in seq(2,22,by=1)){
#   mds.pick<-gam.mds(aral.dat,pred.grid,bnd,grid.res=c(20,20),mds.dim=expl,bs="ds",old.obj=mds2,family=Gamma(link="log"))
#   res<-rbind(c(expl,summary(mds.pick$gam)$sp.criterion),res)
#}
#
#
#
#plot(res,xlab="MDS dimension",ylab="GCV Score",pch=19,cex=0.3)
#abline(h=min(res[,2]),col="red")



