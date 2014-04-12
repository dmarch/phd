# Description: creates the background for the cover of my PhD

library(raster)

# create raster from scratch
r <- raster(nrows=25, ncols=19,xmn=0,xmx=19,ymn=0,ymx=25)
r[]<-rnorm(ncell(r),2,2)
r[]<-rev(seq(1,ncell(r),1))

set.seed(12345)
r <- calc(r, fun=function(x) rnorm(1, x, 30))

# create ramp color
library(RColorBrewer)
palette <- brewer.pal(9,"Blues")[2:9]

# plot
png("cover_background.png", width=19, height=24.5, units="cm", res=300)
op <- par(mar=c(0,0,0,0))
image(r, col = palette,axes=FALSE,xlab="",ylab="")
grid(nx=19, ny=25, lwd=0.8, lty=1, col="white")
text(x=10, y=23, pos=1, "David March Morla",col="white",family="sans",font=2, cex=1.5)
text(x=1, y=5.5, pos=4, labels="Geospatial modelling in marine", col=palette[8],family="sans",font=2, cex=2.4)
text(x=1, y=4, pos=4, "recreational fisheries science",col=palette[8],family="sans",font=2, cex=2.4)

par(op)
dev.off()