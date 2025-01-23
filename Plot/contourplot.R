r1<-rbeta(1000,2,129)
r2<-rbeta(1000,8,112)

library(MASS)
den<-kde2d(r1, r2, n=1000)
n=1000
a<-list("x"=r1,"y"=r2)

z <- array()
for (i in 1:n){
  z.x <- max(which(den$x < r1[i]))
  z.y <- max(which(den$y < r2[i]))
  z[i] <- den$z[z.x, z.y]
}
confidence.border <- quantile(z, probs=.05, na.rm = TRUE)# 0.05
corresponds to 0.95 in draw.contour

plot(r1,r2)
#draw.contour(a, alpha=0.95)
par(new=TRUE)
contour(den, levels=confidence.border, col = "red", add = TRUE)


f#########################
draw.contour<-function(a,alpha=0.95,plot.dens=FALSE, line.width=2,
                       line.type=1, limits=NULL, density.res=800,spline.smooth=-1,...){
  ## a is a list or matrix of x and y coordinates (e.g., a=list("x"=rnorm(100),"y"=rnorm(100)))
## if a is a list or dataframe, the components must be labeled "x" and "y"
## if a is a matrix, the first column is assumed to be x, the second y
## alpha is the contour level desired
## if plot.dens==TRUE, then the joint density of x and y are plotted,
## otherwise the contour is added to the current plot.
## density.res controls the resolution of the density plot

## A key assumption of this function is that very little probability mass lies outside the limits of
## the x and y values in "a". This is likely reasonable if the number of observations in a is large.

require(MASS)
require(ks)
if(length(line.width)!=length(alpha)){
  line.width <- rep(line.width[1],length(alpha))
}

if(length(line.type)!=length(alpha)){
  line.type <- rep(line.type[1],length(alpha))
}

if(is.matrix(a)){
  a=list("x"=a[,1],"y"=a[,2])
}
##generate approximate density values
if(is.null(limits)){
  limits=c(range(a$x),range(a$y))
}
f1<-kde2d(a$x,a$y,n=density.res,lims=limits)

##plot empirical density
if(plot.dens) image(f1,...)

if(is.null(dev.list())){
  ##ensure that there is a window in which to draw the contour
  plot(a,type="n",xlab="X",ylab="Y")
}

##estimate critical contour value
## assume that density outside of plot is very small

zdens <- rev(sort(f1$z))
Czdens <- cumsum(zdens)
Czdens <- (Czdens/Czdens[length(zdens)])
for(cont.level in 1:length(alpha)){
  ##This loop allows for multiple contour levels
  crit.val <- zdens[max(which(Czdens<=alpha[cont.level]))]
  
  ##determine coordinates of critical contour
  b.full=contourLines(f1,levels=crit.val)
  for(c in 1:length(b.full)){
    ##This loop is used in case the density is multimodal or if the desired contour
    ##  extends outside the plotting region
    
    b=list("x"=as.vector(unlist(b.full[[c]][2])),"y"=as.vector(unlist(b.full[[c]][3])))
    
    ##plot desired contour
    line.dat<-xspline(b,shape=spline.smooth,open=TRUE,draw=FALSE)
    lines(line.dat,lty=line.type[cont.level],lwd=line.width[cont.level])
  }
}
}
