# figure to demonstrate how big the nullspace is in tprs
# compared to using Duchon splines

# for dimension 1 to 100...
d<-1:6

# first tprs
# resitriction that 2m>d+1 => m= (d-1)/2 +1 as a minimum
m<-(d-1)/2+1
tprs.nullspace<-choose(m+d-1,d)

# now for Duchon, m=2, use s to make up the gap
duchon.nullspace<-choose(2+d-1,d)

mmin<-min(c(tprs.nullspace,duchon.nullspace))
mmax<-max(c(tprs.nullspace,duchon.nullspace))

pdf(file="nullspace-dim.pdf",width=3,height=5)


par(mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)

plot(x=d,y=seq(mmin,mmax,len=length(d)),type="n",xlab="Smoothing dimension (d)",ylab="Nullspace dimension (M)")
lines(x=d,y=tprs.nullspace,col="blue")
lines(x=d,y=duchon.nullspace,col="red")


dev.off()
