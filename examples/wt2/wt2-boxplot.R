# make a boxplot of the ramsay results

library(ggplot2)


dat<-read.csv("wt2-res.csv")
dat<-dat[,-1]
names(dat)<-c("model","noise","n","i","mse","mdsdim")

dat<-dat[dat$n==500,]


theme_set(theme_bw())
p<-ggplot(dat)
p<-p+geom_boxplot(aes(x=model,y=log(mse)))
p<-p+facet_grid(noise~n)
p<-p+labs(x="Model",y="Logarithm of mean MSE per realisation")
p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          panel.background=theme_rect())
print(p)

ggsave(file="ramsay-result.eps",height=6,width=6)

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




# (paired) Wilcoxon signed rank test
tprs.dat<-dat[dat$model=="tprs",]
soap.dat<-dat[dat$model=="soap",]
mdsds.dat<-dat[dat$model=="mdsds",]

for(n in unique(tprs.dat$n)){
   for(noise in unique(tprs.dat$noise)){

      this.tprs<-tprs.dat[tprs.dat$n==n & tprs.dat$noise==noise,]
      this.soap<-soap.dat[soap.dat$n==n & soap.dat$noise==noise,]
      this.mdsds<-mdsds.dat[mdsds.dat$n==n & mdsds.dat$noise==noise,]

      p<-wilcox.test(this.tprs$mse,this.soap$mse,paired=TRUE)$p.value
      cat("tprs n=",n,"noise=",noise,
          sign(median(this.soap$mse)-median(this.tprs$mse)),"p=",p,"\n")

      p<-wilcox.test(this.mdsds$mse,this.soap$mse,paired=TRUE)$p.value
      cat("mdsds n=",n,"noise=",noise,
           sign(median(this.soap$mse)-median(this.mdsds$mse)),"p=",p,"\n")

   }
}



median(mdsds.dat$mdsdim)
