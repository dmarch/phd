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

picture <- function(img, x, y, xdim, ydim, frame.offset=0.2, shadow.left=0, shadow.right=0.3,
                    shadow.top=0.1, shadow.bottom=0.3)
{
  polygon(x=c(x+shadow.left,x+xdim+shadow.right,x+xdim+shadow.right,x+shadow.left),
          y=c(y-shadow.bottom,y-shadow.bottom,y+ydim+shadow.top,y+ydim+shadow.top),
          col=shadow,border=NA) # shadow mask
  polygon(x=c(x-frame.offset,x+xdim+frame.offset,x+xdim+frame.offset,x-frame.offset),
          y=c(y-frame.offset,y-frame.offset,y+ydim+frame.offset,y+ydim+frame.offset),
          col=frame,border="white") # white frame
  rasterImage(img,x,y,x+xdim,y+ydim)  
}

# create background raster (presentation)
r <- raster(nrows=19, ncols=25,xmn=0,xmx=25,ymn=0,ymx=19)
r[]<-rev(seq(1,ncell(r),1)) #use here 'rev' if want the gradient in other way
set.seed(12345)
r1 <- calc(r, fun=function(x) rnorm(1, x, 30))  # randomize gradient
r2 <- calc(r, fun=function(x) rev(sqrt(x))/max(sqrt(x)))  # create alpha mask. Use rev(x)/max(x)

# create background raster (dissertation)
r <- raster(nrows=25, ncols=19,xmn=0,xmx=19,ymn=0,ymx=25)
r[]<-rev(seq(1,ncell(r),1)) #use here 'rev' if want the gradient in other way
set.seed(12345)
r1 <- calc(r, fun=function(x) rnorm(1, x, 30))  # randomize gradient
r2 <- calc(r, fun=function(x) rev(x)/max(x))  # create alpha mask. Use rev(x)/max(x)

# create background raster (book)
r <- raster(nrows=25, ncols=39,xmn=0,xmx=39,ymn=0,ymx=25)
r[]<-rev(seq(1,ncell(r),1)) #use here 'rev' if want the gradient in other way
set.seed(12345)
r1 <- calc(r, fun=function(x) rnorm(1, x, 50))  # randomize gradient
r2 <- calc(r, fun=function(x) rev(x)/max(x))  # create alpha mask. Use rev(x)/max(x)



# import pictures
library(png)
img1 <- readJPEG("posidonia.jpg", native=TRUE)
img2 <- readJPEG("barca_imedea.jpg", native=TRUE)
img3 <- readJPEG("scriba.jpg", native=TRUE)
#img4 <- readJPEG("logoimedeacsicuibleyend_horizcolor.jpg", native=TRUE)
img5 <- readPNG("logoalpha.png")

# set colors
palette <- brewer.pal(9,"Blues")[2:9]
frame <- add.alpha("white",1)
grid <- add.alpha("white",0.3)
shadow <- add.alpha("black",0.3)
mask <- sapply(r2[],FUN=add.alpha,COLORS="white")
band <- add.alpha("white",0.9)

# set fonts
windowsFonts(
  calibri=windowsFont("Calibri"),
  minion=windowsFont("Minion Pro"),
  myriad=windowsFont("Myriad Pro"),
  myriadlight=windowsFont("Myriad Pro Light"),
  myriadcond=windowsFont("Myriad Pro Cond"),
  cambria=windowsFont("Cambria"))



### A1) Cover PPT
png("ppt_cover.png", width=25.4, height=19.05, units="cm", res=300)  # alternatively, use tiff or pdf
op <- par(mar=c(0, 0, 0, 0)) 
image(r1, col = palette, axes=FALSE, xlab="", ylab="")  # plot rastr
image(r2, col = mask, add=TRUE)  # plot gradient transparency mask
par(op)
dev.off()

### A2) Front cover design

# background
png("front_cover2.png", width=19, height=24.6, units="cm", res=300)  # alternatively, use tiff or pdf
op <- par(mar=c(0, 0, 0, 0))  # set margins to 0
image(r1, col = palette, axes=FALSE, xlab="", ylab="")  # plot rastr
#grid(nx=19, ny=25, lwd=0.8, lty=1, col=grid)  # plot grid "white"
image(r2, col = mask, add=TRUE)  # plot gradient transparency mask

# pictures
picture(img1, x=1.5, y=13, xdim=4, ydim=4)
picture(img2, x=7.5, y=13, xdim=4, ydim=4)
picture(img3, x=13.5, y=13, xdim=4, ydim=4)

# text
typo <- "myriadcond"
text(x=1.5, y=7.5, pos=4, labels="Geospatial modeling in marine", col=palette[8],family=typo,font=2, cex=3.3)
text(x=1.5, y=6, pos=4, "recreational fisheries science",col=palette[8],family=typo,font=2, cex=3.3)
text(x=1.5, y=4, pos=4, "David March Morlà",col=palette[8],family=typo,font=2, cex=1.5)
text(x=9.5, y=2, pos=1, "PhD Thesis",col=palette[8],family=typo,font=2, cex=1.2) #x=9.5 pos=1 is mid point

# close
par(op)
dev.off()



### B) Back cover design

# background
png("back_cover.png", width=19, height=24.6, units="cm", res=300)  # alternatively, use tiff or pdf
op <- par(mar=c(0, 0, 0, 0))  # set margins to 0
image(r1, col = palette, axes=FALSE, xlab="", ylab="")  # plot rastr
#grid(nx=19, ny=25, lwd=0.8, lty=1, col="white")  # plot grid
image(r2, col = mask, add=TRUE)  # plot gradient transparency mask

# pictures
width=5
xleft=9
ybottom=1
rasterImage(img4,xleft=xleft,ybottom=ybottom,xright=xleft+width,ytop=ybottom+(width*dim(img4)[1])/dim(img4)[2])

# band
#polygon(x=c(3,16,16,3), y=c(14,14,23,23), col=band, border=NA) 

# text
text(x=1.5, y=2, pos=4, "Cover designed using R. Get the code at:",col=palette[8],family=typo,font=2, cex=0.7) #x=9.5 pos=1 is mid point
text(x=1.5, y=1.5, pos=4, "https://github.com/dmarch",col=palette[8],family=typo,font=2, cex=0.7) #x=9.5 pos=1 is mid point

# close
par(op)
dev.off()



### C) Book design

### C1) Front cover design

# background
png("book.png", width=39, height=24.6, units="cm", res=300)  # alternatively, use tiff or pdf
op <- par(mar=c(0, 0, 0, 0))  # set margins to 0
image(r1, col = palette, axes=FALSE, xlab="", ylab="")  # plot rastr
#grid(nx=19, ny=25, lwd=0.8, lty=1, col=grid)  # plot grid "white"
image(r2, col = mask, add=TRUE)  # plot gradient transparency mask

# pictures
offset <- 20
picture(img1, x=offset+1.5, y=13, xdim=4, ydim=4)
picture(img2, x=offset+7.5, y=13, xdim=4, ydim=4)
picture(img3, x=offset+13.5, y=13, xdim=4, ydim=4)

# text
typo <- "myriadcond"
text(x=offset+1.5, y=7.5, pos=4, labels="Geospatial modeling in marine", col=palette[8],family=typo,font=2, cex=3.3)
text(x=offset+1.5, y=6, pos=4, "recreational fisheries science",col=palette[8],family=typo,font=2, cex=3.3)
text(x=offset+1.5, y=4, pos=4, "David March Morlà",col=palette[8],family=typo,font=2, cex=1.5)
text(x=offset+9.5, y=2, pos=1, "PhD Thesis",col=palette[8],family=typo,font=2, cex=1.2) #x=9.5 pos=1 is mid point


### C2) Back  cover design

# pictures
width=5
xleft=9
ybottom=1
rasterImage(img5,xleft=xleft,ybottom=ybottom,xright=xleft+width,ytop=ybottom+(width*dim(img4)[1])/dim(img4)[2])

# text
text(x=1.5, y=2, pos=4, "Cover designed using R. Get the code at:",col=palette[8],family=typo,font=2, cex=0.7) #x=9.5 pos=1 is mid point
text(x=1.5, y=1.5, pos=4, "https://github.com/dmarch",col=palette[8],family=typo,font=2, cex=0.7) #x=9.5 pos=1 is mid point


### C3) Gutter

# band
polygon(x=c(19,20,20,19), y=c(0,0,25,25), col=palette[5], border=NA) 
text(x=19.5, y=1, adj=0, srt=90, "David March Morlà",col="white",family=typo,font=2, cex=1) #x=9.5 pos=1 is mid point
text(x=19.5, y=6.8, adj=0, srt=90, "Geospatial modeling in marine recreational fisheries science",col="white",family=typo,font=2, cex=1.2) #x=9.5 pos=1 is mid point
text(x=19.5, y=22, adj=0, srt=90, "PhD Thesis",col="white",family=typo,font=2, cex=1) #x=9.5 pos=1 is mid point


# close
par(op)
dev.off()
