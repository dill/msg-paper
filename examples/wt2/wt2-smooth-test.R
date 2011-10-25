# function to run simulations on the wigglytop 2 domain
# David Lawrence Miller 2009-2011
 
wt2_smooth_test<-function(samp.size=250,noise.level=0.05,plot.it=FALSE,
                          gendata,bnd,soap.knots){
 
   # create the sample index
   samp.ind<-sample(1:length(gendata$x),samp.size)

   ## create the sample
   gendata.samp<- list(x=gendata$x[samp.ind],
                       y=gendata$y[samp.ind],
                       z=gendata$z[samp.ind]+noise.level*rnorm(length(samp.ind)))

   gendata<-list(x=gendata$x[-samp.ind],
                  y=gendata$y[-samp.ind],
                  z=gendata$z[-samp.ind])


   #  tprs
   b.tp<-gam(z~s(x,y,k=140),data=gendata.samp)
   fv.tp<-predict(b.tp,newdata=gendata)

   # fit MDS/Duchon 95%
   mdsds<-gam.mds(gendata.samp,gendata,bnd,grid.res=120,gam.method="GCV.Cp",k=140)
   fv.mdsds<-mdsds$pred

   # soap
   b.soap<-gam(z~s(x,y,k=60,bs="so",xt=list(bnd=list(bnd))),
               knots=soap.knots,data=gendata.samp)
   fv.soap <- predict(b.soap,newdata=gendata)

   ### calculate MSEs
   mses<-c(mean((fv.tp-gendata$z)^2,na.rm=T), 
           mean((fv.mdsds-gendata$z)^2,na.rm=T), 
           mean((fv.soap-gendata$z)^2,na.rm=T),
           mdsds$mds.dim) 


   return(mses)
}
