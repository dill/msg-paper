# run simulations on the Ramsay horseshoe
# compares:
#  1 - tprs
#  2 - soap film
#  3 - mdsds
#  4 - gltps (a la Wang and Ranalli)

library(mgcv)
library(soap)
library(msg)
library(fields)

# source in the files for W+R
source("wr-wrapper.R")
source("tps.R")


# let's do this in parallel!
library(doMC)
library(foreach)
options(cores=6)
registerDoMC()

## options
noise.levels<-c(0.1,1,10) # as in soap paper
sample.sizes<-c(100, 300, 600) # 600 used in soap paper
nsims<-200


## Initialisation
# make the horseshoe boundary
bnd <- fs.boundary()
bnd<-pe(bnd,seq(1,length(bnd$x),8)) # simplify the boundary
fs.bnd<-list(x=c(bnd$x,bnd$x[1]),y=c(bnd$y,bnd$y[1]))

# create the prediction grid and the points to sample
# from
m<-45;n<-25
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
big.res<-c()

# simulation time!
for(noise.level in noise.levels){
   for(sample.size in sample.sizes){

      result<-foreach(j=1:nsims,.combine=rbind,.init=c())%dopar%{
#for(j in 1:nsims){
         # this.res<-c(model, noise, samp, j, MSE, MDS dimension)

         # make the sample
         samp.ind<-sample(1:length(fs.data$x),sample.size)
         samp<-fs.data[samp.ind,]
         samp$z<-samp$z+rnorm(sample.size,0,noise.level)


         ## fit tprs
         b.tprs<-gam(z~s(x,y,k=100),data=samp)
         fv.tprs<-predict(b.tprs,pred.data)
         this.res<-c("tprs",noise.level,sample.size,j,
                     mean((fv.tprs-fs.data$z)^2),0)

         # fit soap film
         b.soap<-gam(z~s(x,y,k=39,bs="so",xt=list(bnd=list(fs.bnd))),
                     knots=knots,data=samp)
         fv.soap<-predict(b.soap,newdata=pred.data,block.size=-1)
         this.res<-rbind(this.res,c("soap",noise.level,sample.size,j,
                         mean((fv.soap-fs.data$z)^2),0))

         # fit mdsds
         mds.fit<-gam.mds(samp,pred.data,fs.bnd,grid.res=c(20,20),
                          gam.method="GCV.Cp") 
         this.res<-rbind(this.res,c("mdsds",noise.level,sample.size,j,
                         mean((mds.fit$pred-fs.data$z)^2),mds.fit$mds.dim))


         # fit W+R
         n.knots<-40 # as in the paper
         xk<-cover.design(matrix(c(samp$x,samp$y),length(samp$x),2),n.knots)
         xk<-matrix(c(xk[,1],xk[,2]),length(xk[,1]),2)
         beta.wr<-wr(samp,list(x=xk[,1],y=xk[,2]),fs.bnd)
         pred.wr<-wr.pred(pred.data,
                          list(x=xk[,1],y=xk[,2]),beta.wr,fs.bnd)
         this.res<-rbind(this.res,c("wr",noise.level,sample.size,j,
                         mean((pred.wr-fs.data$z)^2),0))

         this.res
      }

     big.res<-rbind(big.res,result)

   }
}

write.csv(big.res,"ramsay-results.csv")

