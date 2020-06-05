pkgname <- "OSMscale"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('OSMscale')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("OSMscale-package")
### * OSMscale-package

flush(stderr()); flush(stdout())

### Name: OSMscale-package
### Title: Add a Scalebar to OpenStreetMap Plots
### Aliases: OSMscale-package OSMscale
### Keywords: documentation package

### ** Examples






cleanEx()
nameEx("biketrack")
### * biketrack

flush(stderr()); flush(stdout())

### Name: biketrack
### Title: GPS recorded bike track
### Aliases: biketrack
### Keywords: datasets

### ** Examples


data(biketrack)
plot(biketrack[,1:2])
# see equidistPoints




cleanEx()
nameEx("checkLL")
### * checkLL

flush(stderr()); flush(stdout())

### Name: checkLL
### Title: lat-long coordinate check
### Aliases: checkLL

### ** Examples

checkLL(lat=52, long=130)
checkLL(130, 52, fun=message)
checkLL(85:95, 0, fun=message)

d <- data.frame(x=0, y=0)
checkLL(y,x, d)

# informative errors:
library("berryFunctions")
is.error(   checkLL(85:95, 0, fun="message"),  tell=TRUE)
is.error(   checkLL(170,35),  tell=TRUE)

mustfail <- function(expr) stopifnot(berryFunctions::is.error(expr))
mustfail( checkLL(100)         )
mustfail( checkLL(100, 200)    )
mustfail( checkLL(-100, 200)   )
mustfail( checkLL(90.000001, 0)   )




cleanEx()
nameEx("degree")
### * degree

flush(stderr()); flush(stdout())

### Name: degree
### Title: decimal degree coordinate conversion
### Aliases: degree
### Keywords: character spatial

### ** Examples

# DECIMAL to DMS notation: --------------------------------------------------
degree(52.366360, 13.024181)
degree(c(52.366360, -32.599203), c(13.024181,-55.809601))
degree(52.366360, 13.024181, drop=TRUE) # vector
degree(47.001, -13.325731, digits=5)

# Use table with values instead of single vectors:
d <- read.table(header=TRUE, sep=",", text="
lat, long
 52.366360,  13.024181
-32.599203, -55.809601")
degree(lat, long, data=d)

# DMS to DECIMAL notation: --------------------------------------------------
# You can use the degree symbol and escaped quotation mark (\") as well.
degree("52'21'58.9'N", "13'1'27.1'E")
print(degree("52'21'58.9'N", "13'1'27.1'E"), digits=15)

d2 <- read.table(header=TRUE, stringsAsFactors=FALSE, text="
lat long
52'21'58.9'N 13'01'27.1'E
32'35'57.1'S 55'48'34.6'W") # columns cannot be comma-separated!
degree(lat, long, data=d2)

# Rounding error checks: ----------------------------------------------------
oo <- options(digits=15)
d
degree(lat, long, data=degree(lat, long, d))
degree(lat, long, data=degree(lat, long, d, digits=3))
options(oo)
stopifnot(all(degree(lat,long,data=degree(lat,long,d, digits=3))==d))




cleanEx()
nameEx("earthDist")
### * earthDist

flush(stderr()); flush(stdout())

### Name: earthDist
### Title: distance between lat-long coordinates
### Aliases: earthDist
### Keywords: spatial

### ** Examples

d <- read.table(header=TRUE, sep=",", text="
lat, long
52.514687,  13.350012   # Berlin
51.503162,  -0.131082   # London
35.685024, 139.753365") # Tokio
earthDist(lat, long, d)      # from Berlin to L and T: 928 and 8922 km
earthDist(lat, long, d, i=2) # from London to B and T: 928 and 9562 km

# slightly different with other formulas:
# install.packages("geosphere")
# geosphere::distHaversine(as.matrix(d[1,2:1]), as.matrix(d[2,2:1])) / 1000


# compare with UTM distance
set.seed(42)
d <- data.frame(lat=runif(100, 47,54), long=runif(100, 6, 15))
d2 <- projectPoints(d$lat, d$long)
d_utm <- berryFunctions::distance(d2$x[-1],d2$y[-1], d2$x[1],d2$y[1])/1000
d_earth <- earthDist(lat,long, d)[-1]
plot(d_utm, d_earth) # distances in km
hist(d_utm-d_earth) # UTM distance slightly larger than earth distance
plot(d_earth, d_utm-d_earth) # correlates with distance
berryFunctions::colPoints(d2$x[-1], d2$y[-1], d_utm-d_earth, add=FALSE)
points(d2$x[1],d2$y[1], pch=3, cex=2, lwd=2)





cleanEx()
nameEx("equidistPoints")
### * equidistPoints

flush(stderr()); flush(stdout())

### Name: equidistPoints
### Title: Evenly spaced points along path
### Aliases: equidistPoints
### Keywords: spatial

### ** Examples

library(berryFunctions) # distance, colPoints etc
x <- c(2.7, 5, 7.8, 10.8, 13.7, 15.8, 17.4, 17.7, 16.2, 15.8, 15.1, 13.1, 9.3, 4.8, 6.8, 12.2)
y <- c(2.3, 2.1, 2.6, 3.3, 3.7, 4.7, 7.6, 11.7, 12.4, 12.3, 12.3, 12.3, 12, 12.1, 17.5, 19.6)
eP <- equidistPoints(x,y, n=10) ; eP
plot(x,y, type="o", pch=4)
points(equidistPoints(x,y, n=10), col=4, pch=16)
points(equidistPoints(x,y, n=10, nint=1), col=2) # from original point set
round(distance(eP$x, eP$y), 2) # the 2.69 instead of 4.50 is in the sharp curve
# These points are quidistant along the original track

plot(x,y, type="o", pch=16, col=2)
round(sort(distance(x,y)), 2)
xn <- equidistPoints(x,y, n=10)$x
yn <- equidistPoints(x,y, n=10)$y
lines(xn,yn, type="o", pch=16)
round(sort(distance(xn,yn)), 2)
for(i in 1:8)
{
xn <- equidistPoints(xn,yn, n=10)$x
yn <- equidistPoints(xn,yn, n=10)$y
lines(xn,yn, type="o", pch=16)
print(round(sort(distance(xn,yn)), 2))
} # We may recursively get closer to equidistant along track _and_ air,
# but never actually reach it.

# Real dataset:
data(biketrack)
colPoints(lon, lat, ele, data=biketrack, add=FALSE, asp=1, pch=4, lines=TRUE)
points(equidistPoints(lon, lat, data=biketrack, n=25), pch=3, lwd=3, col=2)
bt2 <- equidistPoints(lon, lat, ele, data=biketrack, n=25)
bt2$dist <- distance(bt2$x, bt2$y)*1000
colPoints(x, y, z, data=bt2, legend=FALSE)
# in curves, crow-distance is shorter sometimes
plot(lat~lon, data=biketrack, asp=1, type="l")
colPoints(x, y, dist, data=bt2, Range=c(2.5,4), add=TRUE, asp=1, pch=3, lwd=5)
lines(lat~lon, data=biketrack)




cleanEx()
nameEx("maxEarthDist")
### * maxEarthDist

flush(stderr()); flush(stdout())

### Name: maxEarthDist
### Title: maximum distance between set of points
### Aliases: maxEarthDist
### Keywords: spatial

### ** Examples


d <- read.table(header=TRUE, text="
    x     y
9.19 45.73
6.55 58.13
7.71 71.44")

plot(d, asp=1, pch=as.character(1:3))
earthDist(y,x,d, i=2)
earthDist(y,x,d, i=3)

maxEarthDist(y,x,d)




cleanEx()
nameEx("pointsMap")
### * pointsMap

flush(stderr()); flush(stdout())

### Name: pointsMap
### Title: Get map for lat-long points
### Aliases: pointsMap
### Keywords: hplot spatial

### ** Examples

if(interactive()){
d <- read.table(sep=",", header=TRUE, text=
"lat, long # could e.g. be copied from googleMaps, rightclick on What's here?
43.221028, -123.382998
43.215348, -123.353804
43.227785, -123.368694
43.232649, -123.355895")

map <- pointsMap(lat, long, data=d)
map_utm <- pointsMap(lat, long, d, map=map, proj=putm(d$long))
axis(1); axis(2) # now in meters
projectPoints(d$lat, d$long)
scaleBar(map_utm, x=0.2, y=0.8, unit="mi", type="line", col="red", length=0.25)
pointsMap(lat, long, d[1:2,], map=map_utm, add=TRUE, col="red", pch=3, pargs=list(lwd=3))

d <- data.frame(long=c(12.95, 12.98, 13.22, 13.11), lat=c(52.40,52.52, 52.36, 52.45))
map <- pointsMap(lat,long,d, type="bing") # aerial map
}




cleanEx()
nameEx("proj")
### * proj

flush(stderr()); flush(stdout())

### Name: proj
### Title: CRS of various PROJ.4 projections
### Aliases: proj putm posm pll posm pll
### Keywords: spatial

### ** Examples

posm()
str(posm())
posm()@projargs
pll()
putm(5:14) # Germany
putm(zone=33) # Berlin




cleanEx()
nameEx("projectPoints")
### * projectPoints

flush(stderr()); flush(stdout())

### Name: projectPoints
### Title: Project lat-lon points
### Aliases: projectPoints
### Keywords: spatial

### ** Examples

library("OpenStreetMap")
lat <- runif(100, 6, 12)
lon <- runif(100, 48, 58)
plot(lat,lon)
plot(projectMercator(lat,lon), main="Mercator")
plot(projectPoints(lat,lon), main="UTM32")
stopifnot(all( projectPoints(lat,lon, to=posm()) == projectMercator(lat,lon) ))

projectPoints(c(52.4,NA),      c(13.6,12.9))
projectPoints(c(52.4,NA),      c(13.6,12.9), quiet=TRUE)
projectPoints(c(52.4,52.3,NA), c(13.6,12.9,13.1))
projectPoints(c(52.4,52.3,NA), c(13.6,NA  ,13.1))
projectPoints(c(52.4,52.3,NA), c(NA  ,12.9,13.1))

# Reference system ETRS89 with GRS80-Ellipsoid (common in Germany)
set.seed(42)
d <- data.frame(N=runif(50,5734000,6115000), E=runif(50, 33189000,33458000))
d$VALUES <- berryFunctions::rescale(d$N, 20,40) + rnorm(50, sd=5)
head(d)
c1 <- projectPoints(lat=d$N, long=d$E-33e6, to=pll(),
          from=sp::CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs") )
c2 <- projectPoints(y, x, data=c1, to=posm() )
head(c1)
head(c2)

## Not run: 
##D  # not checked on CRAN because of file opening
##D map <- pointsMap(y,x, c1, plot=FALSE)
##D pdf("ETRS89.pdf")
##D par(mar=c(0,0,0,0))
##D plot(map)
##D rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
##D      col=berryFunctions::addAlpha("white", 0.7))
##D scaleBar(map, y=0.2, abslen=100)
##D points(c2)
##D berryFunctions::colPoints(c2$x, c2$y, d$VALUE )
##D dev.off()
##D system2("open", "ETRS89.pdf")  # on Linux, try "xdg-open"
##D #unlink("ETRS89.pdf")
## End(Not run)




cleanEx()
nameEx("randomPoints")
### * randomPoints

flush(stderr()); flush(stdout())

### Name: randomPoints
### Title: Distanced random points
### Aliases: randomPoints
### Keywords: datagen spatial

### ** Examples


P <- randomPoints(xmin=200,xmax=700, ymin=300,ymax=680, number=60,mindist=10, asp=1)
rect(xleft=200, ybottom=300, xright=700, ytop=680, col=NA, border=1)

format( round(P,4), trim=FALSE)

for(i in 1:10)
{
rp <- randomPoints(xmin=0,xmax=20, ymin=0,ymax=20, number=20, mindist=3, plot=FALSE)
plot(rp, las=1, asp=1, pch=16)
abline(h=0:30*2, v=0:30*2, col=8); box()
for(i in 1:nrow(rp))
    berryFunctions::circle(rp$x[i],rp$y[i], r=3, col=rgb(1,0,0,alpha=0.2), border=NA)
}




cleanEx()
nameEx("scaleBar")
### * scaleBar

flush(stderr()); flush(stdout())

### Name: scaleBar
### Title: scalebar for OSM plots
### Aliases: scaleBar
### Keywords: aplot spatial

### ** Examples

if(interactive()){
d <- data.frame(long=c(12.95, 12.98, 13.22, 13.11), lat=c(52.40,52.52, 52.36, 52.45))
map <- pointsMap(lat,long,d, scale=FALSE, zoom=9)
coord <- scaleBar(map)  ; coord
scaleBar(map, bg=berryFunctions::addAlpha("white", 0.7))
scaleBar(map, 0.3, 0.05, unit="m", length=0.45, type="line")
scaleBar(map, 0.3, 0.5, unit="km", abslen=5, col=4:5, lwd=3)
scaleBar(map, 0.3, 0.8, unit="mi", col="red", targ=list(col="blue", font=2), type="line")

# I don't like subdivisions, but if you wanted them, you could use:
scaleBar(map, 0.12, 0.28, abslen=10, adj=c(0.5, -1.5)  )
scaleBar(map, 0.12, 0.28, abslen=4, adj=c(0.5, -1.5), targs=list(col="transparent"), label="" )
}

## Not run: 
##D  # don't download too many maps in R CMD check
##D d <- read.table(header=TRUE, sep=",", text="
##D lat, long
##D 52.514687,  13.350012   # Berlin
##D 51.503162,  -0.131082   # London
##D 35.685024, 139.753365") # Tokio
##D map <- pointsMap(lat, long, d, zoom=2, abslen=5000, y=0.7)
##D scaleBar(map, y=0.5, abslen=5000)   # in mercator projections, scale bars are not
##D scaleBar(map, y=0.3, abslen=5000)   # transferable to other latitudes
##D 
##D map_utm <- pointsMap(lat, long, d[1:2,], proj=putm(long=d$long[1:2]),
##D                      zoom=4, y=0.7, abslen=500)
##D scaleBar(map_utm, y=0.5, abslen=500) # transferable in UTM projection
##D scaleBar(map_utm, y=0.3, abslen=500)
## End(Not run)

## Not run: 
##D  ## Too much downloading time, too error-prone
##D # Tests around the world
##D par(mfrow=c(1,2), mar=rep(1,4))
##D long <- runif(2, -180, 180) ;  lat <- runif(2, -90, 90)
##D long <- 0:50 ;  lat <- 0:50
##D map <- pointsMap(lat, long)
##D map2 <- pointsMap(lat, long, map=map, proj=putm(long=long))
## End(Not run)

## Not run: 
##D  ## excluded from tests to avoid package dependencies
##D berryFunctions::require2("SDMTools")
##D berryFunctions::require2("raster")
##D berryFunctions::require2("mapmisc")
##D par(mar=c(0,0,0,0))
##D map <- OSMscale::pointsMap(long=c(12.95, 13.22), lat=c(52.52, 52.36))
##D SDMTools::Scalebar(x=1443391,y=6889679,distance=10000)
##D raster::scalebar(d=10000, xy=c(1443391,6884254))
##D OSMscale::scaleBar(map, x=0.35, y=0.45, abslen=5)
##D library(mapmisc) # otherwise rbind for SpatialPoints is not found
##D mapmisc::scaleBar(map$tiles[[1]]$projection, seg.len=10, pos="center", bg="transparent")
## End(Not run)




cleanEx()
nameEx("triangleArea")
### * triangleArea

flush(stderr()); flush(stdout())

### Name: triangleArea
### Title: Area of a triangle
### Aliases: triangleArea
### Keywords: spatial

### ** Examples


a <- c(1,5.387965,9); b <- c(1,1,5)
plot(a[c(1:3,1)], b[c(1:3,1)], type="l", asp=1)#; grid()

triangleArea(a,b)
#triangleArea(a,b[1:2])




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
