# thin plate spline functions

eta <- function(r) { 
   # thin plate spline basis functions
   ind <- r<=0 
   eta <- r 
   eta[!ind] <- r[!ind]^2*log(r[!ind])/(8*pi) 
   eta[ind] <- 0 
   eta
}

XSC <- function(x,xk=x,D.xxk=NULL,D.xkxk=NULL) { 
   # set up t.p.s., given covariates, x, and knots, xk
   n <- nrow(x);k <- nrow(xk) 
   X <- matrix(1,n,k+3) # tps model matrix 

   if(!is.null(D.xxk) & !is.null(D.xkxk)){
      for (j in 1:k) {
         r<-D.xxk[,j]
         X[,j] <- eta(r)
      }
   }else{
      for (j in 1:k) {
         r <- sqrt((x[,1]-xk[j,1])^2+(x[,2]-xk[j,2])^2) 
         X[,j] <- eta(r)
      }
   }

   X[,j+2] <- x[,1];X[,j+3] <- x[,2] 
   C <- matrix(0,3,k+3) # tps constraint matrix 
   S <- matrix(0,k+3,k+3)# tps penalty matrix 

   if(!is.null(D.xxk) & !is.null(D.xkxk)){
      for (i in 1:k) {
         C[1,i]<-1
         C[2,i] <- xk[i,1]
         C[3,i] <- xk[i,2] 
         for (j in i:k){ 
            S[j,i]<-S[i,j]<-eta(D.xkxk[i,j])
         }
      }
   }else{
      for (i in 1:k) {
         C[1,i]<-1;C[2,i] <- xk[i,1];C[3,i] <- xk[i,2] 
         for (j in i:k){ 
            S[j,i]<-S[i,j]<-eta(sqrt(sum((xk[i,]-xk[j,])^2))) 
         }
      }
   }
   list(X=X,S=S,C=C)
}

absorb.con <- function(X,S,C) { 
   # get constraint null space, Z...
   qrc <- qr(t(C)) # QR=C’, Q=[Y,Z] 
   m <- nrow(C);k <- ncol(X) 
   X <- t(qr.qty(qrc,t(X)))[,(m+1):k] # form XZ 
   # now form Z’SZ ... 
   S <- qr.qty(qrc,t(qr.qty(qrc,t(S))))[(m+1):k,(m+1):k]
   list(X=X,S=S,qrc=qrc)
}

fit.tps <- function(y,x,xk=x,lambda=NULL,D.xxk=NULL,D.xkxk=NULL) { 
   tp <- XSC(x,xk,D.xxk,D.xkxk)	# get tps matrices
   tp <- absorb.con(tp$X,tp$S,tp$C) # make unconstrained 
#   ev <- eigen(tp$S,symmetric=TRUE) # get sqrt penalty, rS 
#   rS <- ev$vectors%*%(ev$values^.5*t(ev$vectors)) 
   rS<-sqrt(diag(nrow(xk)))
   rS[(nrow(xk)-2):nrow(xk),(nrow(xk)-2):nrow(xk)]<-0

   n<-nrow(x)


   # objective function for optim to use
   gcv.objfcn<-function(lambda,tp,y,rS,n){

      lambda<-exp(lambda)

      # as above...
      X <- rbind(tp$X,rS*sqrt(lambda)) 
      z <- c(y,rep(0,ncol(rS)))
      mod<-lm(z~X-1)

      # adapted from p132 red book
      n<-length(y)
      trA<-sum(influence(mod)$hat[1:n])
      rss<-sum((y-fitted(mod)[1:n])^2)
      
      return(n*rss/(n-trA)^2)
   }

   # do the optimisation
   opt<-optimize(gcv.objfcn,tp=tp,y=y,rS=rS,lower=log(10^-9),upper=log(10^9),n=n)

#   # plot
#   V<-rep(0,100)
#   olam<-c()
#   lambda<- 10^-9
#   for(i in 1:100){
#      V[i]<-gcv.objfcn(log(lambda),tp=tp,y=y,rS=rS)
#      olam<-c(olam,log(lambda))
#      lambda<-lambda*1.5
#   }
#   X11()
#   plot(olam,V,type="l",xlab="log(lambda)",ylab="GCV")  
#   lambda<-V[which.min(V)]
#   X11()

   # grab the max
   lambda<-exp(opt$minimum)

   # DEBUG - print the GCV score, smoothing parameter
   #cat("lambda=",lambda,"log(lambda)=",opt$minimum," GCV score=",opt$objective,"\n")
   
   # return the fit with the max
   X <- rbind(tp$X,rS*sqrt(lambda)) # augmented model matrix 
   z <- c(y,rep(0,ncol(rS)))	# augmented data 
   mod<-lm(z~X-1)
   beta<-coef(mod)	# fit model
   trA<-sum(influence(mod)$hat[1:n])
   beta<-qr.qy(tp$qrc,c(0,0,0,beta)) # backtransform beta

   # useful attributes
   attr(beta,"knots")<-list(x=xk[,1],y=xk[,2])
   attr(beta,"edf")<-trA

   # give beta a bit of class
   class(beta)<-"mytps"

   return(beta)
}

eval.tps <- function(xp,beta,xk,D.xpxk=NULL) { 
   # evaluate tps at xp, given parameters, beta, and knots, xk.
   k <- nrow(xk);n <- nrow(xp) 
   f <- rep(beta[k+1],n)
   if(!is.null(D.xpxk)){
      for (i in 1:k) { 
         r<-D.xpxk[,i]
         f <- f + beta[i]*eta(r)
      } 
   }else{
      for (i in 1:k) { 
         r <- sqrt((xp[,1]-xk[i,1])^2+(xp[,2]-xk[i,2])^2) 
         f <- f + beta[i]*eta(r)
      } 
   }
   f <- f + beta[k+2]*xp[,1] + beta[k+3]*xp[,2]
}

# wrapper so I can use predict syntax
predict.mytps<-function(beta,data){

   if(is.list(data)){
      data<-matrix(c(pp$x,pp$y),length(pp$x),2)
   }

   knots<-attr(beta,"knots")
   knots<-matrix(c(knots$x,knots$y),length(knots$x),2)

   return(eval.tps(data,beta,knots))
}  

Predict.matrix.tps <- function(beta,xp,D.xpxk=NULL) { 
   # evaluate tps at xp, given parameters, beta, and knots, xk.
   # but return evaluations of basis functions
   # see Predict.matrix() from mgcv
   if(is.list(xp)|is.data.frame(xp)){
      xp<-matrix(c(xp$x,xp$y),length(xp$x),2)
   }
   k <- nrow(xk);n <- nrow(xp) 

   # deal with the knots
   xk<-attr(beta,"knots")
   xk<-matrix(c(xk$x,xk$y),length(xk$x),2)

   # matrix to hold results
   f<-matrix(NA,n,k+3)

   if(!is.null(D.xpxk)){
      for (i in 1:k) { 
         r<-D.xpxk[,i]
         f[,i] <- beta[i]*eta(r)
      } 
   }else{
      for (i in 1:k) { 
         r <- sqrt((xp[,1]-xk[i,1])^2+(xp[,2]-xk[i,2])^2) 
         f[,i]<-beta[i]*eta(r)
      } 
   }

   # unpenalised part
   f[,k+1]<-rep(beta[k+1],n)
   f[,k+2]<-beta[k+2]*xp[,1]
   f[,k+3]<-beta[k+3]*xp[,2]

   return(f)
}
