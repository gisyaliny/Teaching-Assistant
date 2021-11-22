######################################################################
## Mandelbrot set
## see: https://en.wikipedia.org/wiki/Mandelbrot_set
## watch at your own risk: https://www.youtube.com/watch?v=u_P83LcI8Oc
######################################################################

aLimits <- bLimits <- c(-2,0.8)
MaxIter=25
cl=colours()
aStep=seq(aLimits[1],aLimits[2],by=0.005)
bStep=seq(bLimits[1],bLimits[2],by=0.005)
S=floor(length(cl)/MaxIter)
Dist=0
PointsMatrix=array(0,dim=c(length(aStep)*length(bStep),3))
t=0
for(a in aStep) {
  for(b in bStep+0.6) {
    x=0;y=0;n=0;Dist=0
    while(n<MaxIter & Dist<4) {
      n=n+1
      newx=a+x^2-y^2
      newy=b+2*x*y
      Dist=newx^2+newy^2
      x=newx;y=newy
    }  #end::while
    if(Dist<4) colour=24 # black colour
    else colour=n*S
    t=t+1
    PointsMatrix[t,]=c(a,b,colour)
  } #end::for inner
} # end::for outer

X11(height=12, width=12)   # set graphics window
plot(PointsMatrix[,1], PointsMatrix[,2], col=cl[PointsMatrix[,3]], cex=2, pch=".",
     xlim=aLimits, xlab="Real Component", ylim=bLimits+0.6, ylab="Imaginary Component" )

