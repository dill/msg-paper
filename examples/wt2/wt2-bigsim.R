# do large scale simulations for wt2 - with Duchon splines
# David Lawrence Miller 2009-2011.

library(msg)
library(fields)
source("../ramsay/wr-wrapper.R")
source("../ramsay/tps.R")
source("../ramsay/pe.R")

# parallel options
library(doMC)
library(foreach)

options(cores=6)
registerDoMC()


source("wt2-smooth-test.R")

######################################################
# The following models get tested:
#
#      col.names<-c("tprs","mdsds","soap")


######################################################
# OPTIONS
nsims<-1#200
samp.sizes<-c(250,500)
# noise levels = 0.35,0.9,1.55
# snr = 0.95,0.75,0.50


######################################################
# initial setup
# this pre-calculates some things to make life easier

## create a boundary...
bnd <- read.csv("wt2-bnd.csv")

names(bnd)<-c("x","y")

## Simulate some fitting data, inside boundary...
gendata<-read.csv("wt2-surf.csv",header=TRUE)

gendata<-list(x=gendata$x[gendata$inside==1],
               y=gendata$y[gendata$inside==1],
               z=gendata$z[gendata$inside==1])

na.ind<-!(is.na(gendata$x)&is.na(gendata$y)&is.na(gendata$z))

gendata<-list(x=gendata$x[na.ind],
               y=gendata$y[na.ind],
               z=gendata$z[na.ind])


##########################################################
### setup the soap knots
knots.x<-rep(seq(-2.9,2.9,length.out=15),15)
knots.y<-rep(seq(-2.9,3.6,length.out=15),rep(15,15))
insideknots<-inSide(bnd,knots.x,knots.y)
soap.knots<-data.frame(x=knots.x[insideknots],y=knots.y[insideknots])
soap.knots<-pe(soap.knots,-c(55,96,108))

##########################################################
# actually run the sim now... 

noiselevels<-c(0.35,0.9,1.55)

big.res<-c()
# c(model, noise, samp, j, MSE, MDS dimension)


for(samp.size in samp.sizes){
   for(noise.level in noiselevels){
   
      cat("Noise level=",noise.level,"\n")
   
      # set up some containers
      #res.mse<-matrix(NA,sim.size,3)
      
      # do the sims
#for(j in 1:nsims){
      result<-foreach(j=1:nsims,.combine=rbind,.init=c())%dopar%{
         res<-wt2_smooth_test(samp.size,noise.level,plot.it=FALSE,
                                gendata,bnd,soap.knots)
         #res.mse[i,]<-res
         res<-rbind(c("tprs",noise.level,samp.size,j,res[1],NA),
                    c("mdsds",noise.level,samp.size,j,res[2],res[5]),
                    c("soap",noise.level,samp.size,j,res[3],NA),
                    c("wr",noise.level,samp.size,j,res[4],NA)
                   )
      }
      big.res<-rbind(big.res,result)
      
   }
}
# write the file...
write.csv(big.res,file="wt2-res.csv")

