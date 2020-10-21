data(buoy,package = "ocedata")

theta <- (90 - buoy$direction) * pi/180

u <- buoy$wind*cos(theta)

v <- buoy$wind*sin(theta)

head(buoy)

s <- max(buoy$wind)*c(-1,1)

plot(u,v,xlim = s,ylim = s,asp = 1,xlab = "amplitude",ylab = "area",title("Ocean wave graph"))

library(oce)
data(argo)
head(argo)

plot(u,v,asp=1)

lat <- argo[["latitude"]]
lon <- argo[["longitude"]]
plot(lat,lon,asp=1/cos(pi*mean(range(lat))/180))

set.seed(9999)
x <- runif(100)
y <- x + runif(100)
plot(x,y)
plot(x,y,asp=0.1)

barplot(x,asp=10)
barplot(y,asp = 10)

data(coastlineWorldMedium,package = "ocedata")
cwlon <- coastlineWorldMedium[["longitude"]]
cwlat <- coastlineWorldMedium[["latitude"]]
polygon(cwlon,cwlat,col="gray")
head(coastlineWorldMedium)
library(oce)
data(topo2,package = "ocedata")
head(topo2)
contour(topo2)

x <- seq(0.25,358.25,2)
y <- seq(-89.75,88.25,2)
contour(x,y,topo2,asp =1,levels = c(0,-125),drawlabels = FALSE,xlim = c(100,250),ylim = c(30,90),lty = c(1,2))
image(topo2)
imagep(topo2)
imagep(topo2,colormap = colormap(name = "gmt_globe"))
