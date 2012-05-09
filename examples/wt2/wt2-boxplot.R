# make a boxplot of the ramsay results

library(ggplot2)


dat<-read.csv("wt2-res.csv")
dat<-dat[,-1]
names(dat)<-c("model","noise","n","i","mse","mdsdim")

dat<-dat[dat$n==500,]

dat$model<-as.character(dat$model)
dat$model[dat$model=="wr"]<-"gltps"
dat$model<-as.factor(dat$model)

theme_set(theme_bw())
p<-ggplot(dat)
p<-p+geom_boxplot(aes(x=model,y=log(mse)))
#p<-p+facet_grid(noise~n)
p<-p+facet_wrap(~noise,nrow=1)
p<-p+labs(x="Model",y="Logarithm of mean MSE per realisation")
p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          panel.background=theme_rect())
print(p)

ggsave(file="wt2-result.eps",height=4,width=6)
ggsave(file="wt2-result.pdf",height=4,width=6)

## plot the MDS projection dimension
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




tprs.dat<-dat[dat$model=="tprs",]
soap.dat<-dat[dat$model=="soap",]
mdsds.dat<-dat[dat$model=="mdsds",]
wr.dat<-dat[dat$model=="gltps",]

for(n in unique(tprs.dat$n)){
   for(noise in unique(tprs.dat$noise)){

      this.tprs<-tprs.dat[tprs.dat$n==n & tprs.dat$noise==noise,]
      this.soap<-soap.dat[soap.dat$n==n & soap.dat$noise==noise,]
      this.mdsds<-mdsds.dat[mdsds.dat$n==n & mdsds.dat$noise==noise,]
      this.wr<-wr.dat[wr.dat$n==n & wr.dat$noise==noise,]

      p<-wilcox.test(this.tprs$mse,this.mdsds$mse,paired=TRUE)$p.value
      cat("tprs noise=",noise,
          sign(median(this.mdsds$mse)-median(this.tprs$mse)),"p=",p,"\n")

      p<-wilcox.test(this.wr$mse,this.mdsds$mse,paired=TRUE)$p.value
      cat("gltps noise=",noise,
           sign(median(this.mdsds$mse)-median(this.wr$mse)),"p=",p,"\n")

      p<-wilcox.test(this.soap$mse,this.mdsds$mse,paired=TRUE)$p.value
      cat("soap noise=",noise,
           sign(median(this.mdsds$mse)-median(this.soap$mse)),"p=",p,"\n")
   }
}



median(mdsds.dat$mdsdim)
