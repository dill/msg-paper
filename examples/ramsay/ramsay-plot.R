# make a boxplot of the ramsay results

library(ggplot2)

# do the layout stuff
plot.rows<-2
plot.cols<-1
Layout <- grid.layout(nrow = plot.rows, ncol = plot.cols,
                      widths = unit(rep(3,plot.rows*plot.cols),"null"),
                      heights = unit(rep(3,plot.rows*plot.cols), "null"))
subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)
vplayout <- function(...) {
     grid.newpage()
     pushViewport(viewport(layout = Layout))
}


grid.newpage()
pushViewport(viewport(layout = Layout))




dat<-read.csv("ramsay-results.csv")

dat<-dat[,-1]

names(dat)<-c("model","noise","n","i","mse","mdsdim")


theme_set(theme_bw())

p<-ggplot(dat)
p<-p+geom_boxplot(aes(x=model,y=log(mse)))
p<-p+facet_wrap(~n,nrow=1)
p<-p+labs(x="Model",y="Logarithm of per realisation MSE")
p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          panel.background=theme_rect())
#p<-p+scale_y_log()

print(p)

ggsave(file="ramsay-result.pdf",height=4,width=6)


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
m<-45;n<-25
xm <- seq(-1,3.5,length=m); yn<-seq(-1,1,length=n)
xx <- rep(xm,n); yy<-rep(yn,rep(m,n))
onoff<-inSide(bnd,xx,yy)
xx<-xx[onoff];yy<-yy[onoff]
fs.data<-data.frame(x=xx,y=yy,z=fs.test(xx,yy))
pred.data<-data.frame(x=xx,y=yy)


im<-matrix(NA,m,n)

par(mfrow=c(1,3))

im[onoff]<-dat2[1,]
image(z=im,x=xm,y=yn,col=heat.colors(1000),main="tprs",asp=1,xlab="x",ylab="y",zlim=c(-5,5),las=1)
contour(z=im,x=xm,y=yn,add=T,nlevels=25)

im[onoff]<-dat2[2,]
image(z=im,x=xm,y=yn,col=heat.colors(1000),main="soap",asp=1,xlab="x",ylab="y",zlim=c(-5,5),las=1)
contour(z=im,x=xm,y=yn,add=T,nlevels=25)


im[onoff]<-dat2[3,]
image(z=im,x=xm,y=yn,col=heat.colors(1000),main="mdsds",asp=1,xlab="x",ylab="y",zlim=c(-5,5),las=1)
contour(z=im,x=xm,y=yn,add=T,nlevels=25)



