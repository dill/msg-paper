# plot wt2

library(mgcv)
library(soap)
library(msg)


# plot three parts
# 1) image plot
# 2) point plot
# 3) point plot in MDS space

par(mfrow=c(1,3),mar=c(1,1.5,1,1.5),mgp=c(1.5,0.75,0),oma=c(1,1,1,1))


# load high resolution for image plot
bnd <- read.csv("wt2-bnd-HR.csv",header=TRUE)
wt2<-read.csv("wt2-surf-HR.csv",header=TRUE)


# 1 - image plot
wt2.mat<-matrix(wt2$z,100,100)
wt2.mat[wt2$inside==0]<-NA

axis.vals<-list(x=sort(unique(wt2$x)),y=sort(unique(wt2$y)))
image(axis.vals$x,axis.vals$y,wt2.mat,col=heat.colors(300),asp=1)
contour(axis.vals$x,axis.vals$y,wt2.mat,add=TRUE)
lines(bnd,lwd=2)


# low res for the MDS
bnd <- read.csv("wt2-bnd.csv",header=TRUE)
wt2<-read.csv("wt2-surf.csv",header=TRUE)

# 2 - point plot
wt2.pt<-data.frame(x=wt2$x[wt2$inside==1],y=wt2$y[wt2$inside==1])

plot(wt2.pt,cex=0.2,pch=19,asp=1)
lines(bnd,lwd=2)


# 3 - MDS space plot
D<-create_distance_matrix(wt2.pt$x,wt2.pt$y,bnd,faster=1)
grid.mds<-cmdscale(D,eig=TRUE,k=2,x.ret=TRUE)

plot(grid.mds$points,cex=0.2,pch=19,asp=1)

# copy out to eps
dev.copy2eps(file="wt2-plot.eps",width=8,height=3)
dev.copy2pdf(file="wt2-plot.pdf",width=8,height=3)
