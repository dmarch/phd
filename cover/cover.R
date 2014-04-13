# Description: creates the background for the cover of my PhD

library(raster)
library(jpeg)
library(RColorBrewer)

rm(list=ls(all=TRUE)) #reset data

# Functions
add.alpha <- function(COLORS, ALPHA){
  # Description:
  # Add transparency to a color
  #
  # Arguments:
  # COLORS: eg "red"
  # ALPHA: transparency (between 0 and 1)
  #
  # Source: http://menugget.blogspot.com.es/2012/04/adding-transparent-image-layer-to-plot.html
  if(missing(ALPHA)) stop("provide a value for alpha between 0 and 1")
  RGB <- col2rgb(COLORS, alpha=TRUE)
  RGB[4,] <- round(RGB[4,]*ALPHA)
  NEW.COLORS <- rgb(RGB[1,], RGB[2,], RGB[3,], RGB[4,], maxColorValue = 255)
  return(NEW.COLORS)
}


# create background raster
r <- raster(nrows=25, ncols=19,xmn=0,xmx=19,ymn=0,ymx=25)
r[]<-rnorm(ncell(r),2,2)
r[]<-rev(seq(1,ncell(r),1))
set.seed(12345)
r1 <- calc(r, fun=function(x) rnorm(1, x, 30))  # randomize gradient
r2 <- calc(r, fun=function(x) rev(x)/max(x))  # create alpha mask
  

# import pictures
img <- readJPEG("seagrass.jpg", native=TRUE)


# set colors
palette <- brewer.pal(9,"Blues")[2:9]
frame <- add.alpha("white",1)
shadow <- add.alpha("black",0.3)
mask <- sapply(r2[],FUN=add.alpha,COLORS="white")

# set fonts
windowsFonts(
  calibri=windowsFont("Calibri"),
  cambria=windowsFont("cambria"))

### Cover design

# background
png("cover_background.png", width=19, height=24.6, units="cm", res=300)  # alternatively, use tiff or pdf
op <- par(mar=c(0, 0, 0, 0))  # set margins to 0
image(r1, col = palette, axes=FALSE, xlab="", ylab="")  # plot rastr
grid(nx=19, ny=25, lwd=0.8, lty=1, col="white")  # plot grid
image(r2, col = mask, add=TRUE)  # plot gradient transparency mask

# pictures
polygon(x=c(1-0,5+0.3,5+0.3,1-0),y=c(11-0.3,11-0.3,15+0.1,15+0.1),
        col=shadow,border=NA) # shadow mask
polygon(x=c(1-0.2,5+0.2,5+0.2,1-0.2),y=c(11-0.2,11-0.2,15+0.2,15+0.2),
        col=frame,border="white") # white mask
rasterImage(img,1,11,5,15)

# text
text(x=10, y=23, pos=1, "David March Morlá",col="white",family="cambria",font=2, cex=2)
text(x=1, y=5.5, pos=4, labels="Geospatial modelling in marine", col=palette[8],family="cambria",font=2, cex=2.4)
text(x=1, y=4, pos=4, "recreational fisheries science",col=palette[8],family="cambria",font=2, cex=2.4)
text(x=1, y=2.5, pos=4, "PhD Thesis",col=palette[8],family="cambria",font=1, cex=2)



par(op)
dev.off()