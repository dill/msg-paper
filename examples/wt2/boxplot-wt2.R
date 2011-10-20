# make some boxplots
library(ggplot2)

pdf(file="wt2-results.pdf",width=13.3,height=7.88)

# make the text look better for printout
par(cex.axis=0.75,las=1,mgp=c(2,0.75,0),mar=c(2,3,1,1))

# stick all of the results into one matrix
mses<-matrix(NA,100,0)

# model names
mod.names<-c("tprs","soap","mdsds")

# duchon data - GCV.Cp
ddat<-read.csv(file="bigresults-GCV.Cp.csv")
names(ddat)<-c("n","noise","sim","name","dat")
ddat$noise<-as.factor(ddat$noise)
ddat$dat<-ddat$dat/1300 # column has sum not average
ddat.mse<-ddat$dat[ddat$name=="mse"]
ddat.noise<-ddat$noise[ddat$name=="mse"]

# push the data into the right shape
for(err.lev in c("0.35","0.9","1.55")){

   cols<-c()

   mse<-read.csv(paste("wt2-mse-250-",err.lev,".csv",sep=""))
   mse<-mse[,-1]
   mse<-mse[,-2] # take out mds+tp column
   mse<-mse[1:200,]
   mse<-mse[,-c(3,4,5)]
   # add in Duchon
   tmp<-ddat.mse[ddat.noise==err.lev]
   mse<-cbind(mse,tmp)
#spurious<-c(111,135,157)
#mse<-mse[-spurious,]
   #tmp<-ddat.ML.mse[ddat.ML.noise==err.lev]
   #mse<-cbind(mse,tmp)
# 3 spurious soap results, remove them
spurious<-c(111,135,157)
mse<-mse[-spurious,]

   ## extra Wilcoxon test stuff
   #test.against<-2
   #for(i in 1:ncol(mse)){
   #   if(i!=test.against){
   #      pv<-wilcox.test(mse[,test.against],mse[,i],paired=TRUE)$p.value
   #      med<-median(mse[,i]-mse[,test.against])
   #      if(pv<0.01 & med>0){
   #         cols<-c(cols,rep("red",nrow(mse)))
   #      }else if(pv<0.01 & med<0){
   #         cols<-c(cols,rep("green",nrow(mse)))
   #      }else{
   #         cols<-c(cols,rep("white",nrow(mse)))
   #      }
#  #       cat("soap vs. ",mod.names[i],pv,"\n")
   #   }else{
   #      cols<-c(cols,rep("white",nrow(mse)))
   #   }
   #}

   names(mse)<-mod.names
   mse<-melt(mse)
   names(mse)<-c("method","mse")
   errs<-data.frame(error=rep(err.lev,nrow(mse)))
   #cols<-data.frame(cols=cols)
   mse<-cbind(mse,errs)#,cols)
   mses<-rbind(mses,mse)

}




# log the results
mses$mse<-log(mses$mse)


# do the plot
theme_set(theme_bw())
p<-ggplot(mses)
p<-p+geom_boxplot(aes(x=factor(method),y=mse))#,fill=cols))
p<-p+facet_wrap(~error,nrow=1)
p<-p+scale_fill_manual(value = c("green","red","white"),legend=FALSE) 
p<-p+opts(panel.grid.major=theme_blank(), 
                    panel.grid.minor=theme_blank(), 
                    panel.background=theme_rect())
p<-p+labs(x="Method",y="Logarithm of mean MSE per realisation)")
print(p)

dev.off()
