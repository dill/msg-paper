# save a single example of predictions from the models

library(mgcv)
library(soap)
library(msg)

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
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]
fs.data<-data.frame(x=xx,y=yy,z=fs.test(xx,yy))
pred.data<-data.frame(x=xx,y=yy)

# knot setup for soap, as in paper
knots <- data.frame(x=rep(seq(-.5,3,by=.5),4),
                    y=rep(c(-.6,-.3,.3,.6),rep(8,4)))
knots.ind<-inSide(bnd,x=knots$x,y=knots$y)
knots<-list(x=knots$x[knots.ind],y=knots$y[knots.ind])

# clean up
rm(xx,yy,m,n,bnd)

# simulation time!
noise.level<-1
sample.size<-600

# make the sample
samp.ind<-sample(1:length(fs.data$x),sample.size)
samp<-fs.data[samp.ind,]
samp$z<-samp$z+rnorm(sample.size,0,noise.level)

# fit tprs
b.tprs<-gam(z~s(x,y,k=100),data=samp)
fv.tprs<-predict(b.tprs,pred.data)

# fit soap film
b.soap<-gam(z~s(x,y,k=39,bs="so",xt=list(bnd=list(fs.bnd))),
            knots=knots,data=samp)
fv.soap<-predict(b.soap,newdata=pred.data,block.size=-1)

# fit mdsds
mds.fit<-gam.mds(samp,pred.data,fs.bnd,grid.res=c(20,20),
                 gam.method="GCV.Cp") 

# truth
truth<-fs.test(pred.data$x,pred.data$y)

# squish them together
preds<-rbind(fv.tprs,fv.soap,mds.fit$pred,truth)

write.csv(preds,"ramsay-ex.csv")

