\name{berryFunctions-package}
\alias{berryFunctions-package}
\alias{berryFunctions}
\docType{package}
\title{Berry's functions}
\description{Collection of functions, mainly connected with graphics.\cr
- zoom in X11 graphics and ESRI shapefile plots\cr
- plot rainfall-runoff data and optimize parameters for the unit hydrograph in the linear storage cascade\cr
- fit and plot 13 extreme-value-distributions to estimate discharge at given return periods\cr
- write text to plots on top of colored fields in label size (halo-effect)\cr
- draw scatterplots colored by 3rd dimension (as in image, which only deals with grids)\cr
- draw histograms horizontally\cr
- advancedly label date axes and logarithmic axes\cr
- fit multiple functions (power, reciprocal, exponential, logarithmic, polynomial, rational) by regression\cr
- convert lists to data.frames
}
\note{
At some places you'll find \code{## not run} in the examples.
These code blocks were excluded from checking while building, mainly because they are interactive and need mouseclicks, or because they open another device/file.
\cr Normally, you should be able to run them in an interactive session.
If you do find unexecutable code, please tell me!\cr
Feel free to suggest packages in which these functions would fit well.
I strongly depend on - and therefore welcome - any feedback!
}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2011-2014}
\section{suggests}{evd, lmom, maptools, spatstat}
\examples{
### HYDROLOGY
### SPATIAL
### GRAPHICS
### MISCELLANEOUS

\dontrun{

### HYDROLOGY #################################################################

# draw full climate diagram: climateGraph
climateGraph(temp=c(-9.3,-8.2,-2.8,6.3,13.4,16.8,18.4,17,11.7,5.6,-1,-5.9),
             rain=c(46,46,36,30,31,21,26,57,76,85,59,46))

# Extreme value Statistics (for flood risk estimation): extremeStat
# Replaced (with linear moments to fit 13 distributions) by: extremeStatLmom
JM <- c(61.5, 77.0, 37.0, 69.3, 75.6, 74.9, 43.7, 50.8, 55.6, 84.1, 43.6, 81.9,
60.1, 72.4, 61.6, 94.8, 82.6, 57.2,  63.1, 73.8, 51.3, 93.6, 56.9, 52.1, 40.4,
48.9, 113.6, 35.4, 40.1, 89.6, 47.8, 57.6, 38.9, 69.7, 110.8)
extremeStatLmom(JM)

# superposition of precipitation to simulate Q from P: superPos
N <- c(9,5,2,14,1,3) # [mm/hour]
UH <- c(0.1, 0.4, 0.3, 0.1, 0.1) # [1/h]
superPos(N, UH)

# calculate continuous UH with given n and k: unitHydrograph
plot(0:40, unitHydrograph(n=2,  k=3, t=0:40), type="l")

# estimate parameters for Unit Hydrograph, plot data and simulation: lsc
QOBS <- dbeta(1:40/40, 3, 10) + rnorm(20,,0.2) + c(seq(0,1,len=20), rep(1,20))
PREC <- c(1,1,3,4,5,5,4,3,1,1, rep(0,30))
lsc(PREC, QOBS, area=10)

# Nash-Sutcliffe efficiency: nse
QSIM <- lsc(PREC, QOBS, area=10, returnsim=TRUE, plot=FALSE)
nse(QOBS, QSIM)

# Root Mean Squared Error, e.g. to be minimized: rmse
rmse(QOBS, QSIM)

# R squared (coefficient of determination): rsquare
rsquare(QOBS, QSIM)


### SPATIAL ####################################################################
# Here are some functions that were originally in this package:
browseURL("https://github.com/BerryBoessenkool/shapeInteractive/
           blob/master/README.md")

# arrange points randomly with minimal distance to each other: randomPoints
randomPoints(xmin=5,xmax=15, ymin=20,ymax=30, number=25, mindist=1)

# distance between two points on a plane: distance
A <- c(3,  9,-1)  ;  B <- c(7, -2, 4)
distance(A,B, 3,5)
plot(A,B); points(3,5, col=2, pch=16); segments(3,5, A,B)

# calculate Area of a planar triangle: triangleArea
a <- c(1,5.387965,9); b <- c(1,1,5)
plot(a[c(1:3,1)], b[c(1:3,1)], type="l", asp=1)
abline(v=1:9, h=1:5, col=8,lty=2)
triangleArea(a,b)


### GRAPHICS ##################################################################

# Zoom into graphics: pointZooom
a <- rnorm(90); b <- rexp(90)
windows(record=TRUE) # turn recording on
plot(a,b, las=1)
pointZoom(a,b) # scroll through the plots (Pg Up and Pg Dn) to unzoom again.

# add transparency to colors: addAlpha
NewColors <- addAlpha(c("red","blue","yellow","green", "purple"), 0:200/200)
plot(runif(1000), col=NewColors, pch=16, cex=2)

# draw lines with halo around them: smoothLines
x <- 1:5  ; y <- c(0.31, 0.45, 0.84, 0.43, 0.25)
plot(x,y)
smoothLines(x,y)

# write text with colored shape underneath: textField
plot(rnorm(1e4))
textField(5000, 0, "Here's some great readable text", field="round", cex=1.5)

# draw circle with given radius: circle
plot(1:10, asp=1)
circle(6,5, r=2, col=2, border=4, lwd=3)

# Funnel plot for proportional Data: funnelPlot
X <- c(2, 224,  54,  505, 1,  5, 236,  92,  3, 0) # successful events
N <- c(2, 400, 100, 1000, 2, 10, 500, 200, 10, 2) # possible succeses
funnelPlot(X,N, letters[1:10])

# Table with numbers and corresponding color: tableColVal
Bsp <- matrix(c(21,23,26,27, 18,24,25,28, 14,17,23,23, 16,19,21,25), 
              ncol=4, byrow=TRUE)
colnames(Bsp) <- paste0("Measure", LETTERS[1:4])
rownames(Bsp) <- paste("prod", 8:11, sep="_")
tableColVal(Bsp, pdf=FALSE)

# Scatterpoints colored by 3rd dimension: colPoints
i <- c(22, 40, 80, 45, 60, 63, 30, 70) ;  j <- c(5, 33, 12, 56, 20, 40, 45, 45)
k <- c(135, 155, 120, 105, 140, 130, 190, 110)
colPoints(i,j,k, cex=2, pch="+", add=FALSE)
colPointsHist(z=k)

# Histogramm with bars drawn horizontally: horizHist
ExampleData <- rnorm(50,8,5)+5
hpos <- horizHist(ExampleData)
abline(h=hpos(11), col=2, lwd=2)

# Set ylim so that it does not extend below zero: lim0
val <- c(3.2, 1.8, 4.5, 2.8, 0.1, 2.9) # just some numbers
plot(val, ylim=lim0(val) ) # you don't even have to set yaxs="i" ;-)

# Get nice values and labels to write at logarithmic axes: logVals
exdat <- 10^runif(50, -1, 2)
plot(exdat, log="y", yaxt="n")
lv <- logVals(exdat, base=c(1,2,5) )
abline(h=lv$all, col=8 )
axis(2, lv$vals, lv$labs, las=1)

# Label time axis in date-regular intervals: monthLabs
monthLabs(2014,2014, 3)
plot(as.Date("2013-09-25")+sort(sample(0:150, 30)), 
     cumsum(rnorm(30)), type="o", xaxt="n", ann=F)
axis(1, monthLabs(npm=1), format(monthLabs(npm=1), "\%d.\%m.\n \%Y"),
     mgp=c(3,2,0), cex.axis=0.7 )

# Linear regression: plot data, model and r^2: linReg
a <- 1:30   ; b <- a/2.345+rnorm(30,0,3)
linReg(a,b)

# Fit a wide range of function types to see which one is best: mReg
x <- c(1.3, 1.6, 2.1, 2.9, 4.4, 5.7, 6.6, 8.3, 8.6, 9.5)
y <- c(8.6, 7.9, 6.6, 5.6, 4.3, 3.7, 3.2, 2.5, 2.5, 2.2)
mReg(x,y)

# Moving average with overlapping windows: movAv
a <- cumsum(rnorm(100))
plot(a, type="l", pch=16, las=1)
lines(movAv(a), col=2, lwd=3)


### MISCELLANEOUS #############################################################

# calculate confidence interval (and other EDA values): ci, cie
ci(  c(5:8,3,14) )
cie( c(5:8,3,14) )

# sequence given by range or vector of values: seqR
seqR(range=c(12,6), by=-2)
seqR(range=rnorm(20), by=1)

# Convert list with vectors of unequal length to one single data.frame: l2df
eglist <- list(BB=c(6,9,2,6), KA=1:8, JE=c(-3,2) )
eglist
l2df(eglist)  # names are even kept

# add rows to a data.frame: addRows
MYDF <- data.frame(A=5:3, B=2:4)
addRows(MYDF, 3)

# Show memory size of the biggest objects in MB: lsMem
lsMem()

# Lists with arguments for functions, some defaults: owa
?owa # the example section has a good - wait for it - example!

# install.package and require in on function: require2
require2(ada)

# extract pdf link from google search result url: googleLink2pdf
Link <- paste0("http://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1",
        "&cad=rja&sqi=2&ved=0CDIQFjAA&url=http\%3A\%2F\%2Fcran.r-project.org",
        "\%2Fdoc\%2Fmanuals\%2FR-intro.pdf&ei=Nyl4UfHeOIXCswa6pIC4CA",
        "&usg=AFQjCNGejDwPlor4togQZmQEQv72cK9z8A&bvm=bv.45580626,d.Yms")
googleLink2pdf(Link)

# concatenate textfiles contents unchanged into one file: combineTextfiles
cat("This is Sparta.\nKicking your face.", file="BujakashaBerry1.txt")
cat("Chuck Norris will roundhousekick you.", file="BujakashaBerry2.txt")
combineTextfiles(inFiles=paste0("BujakashaBerry", 1:2, ".txt"), 
                 outFile="BujakashaBerry3.txt")
readLines("BujakashaBerry3.txt")
unlink(paste0("BujakashaBerry", 1:3, ".txt"))

# create .Rd documentation from source code in Berry's format: createDoc
# compare arguments in Rd with source code, saves time Rcmd check: compareDoc

# generate name from "random" sample: nameSample
nameSample("berry")
# Kind regards from
set.seed(1248272); paste(sample(letters,5,rep=T),collapse='')

# wish neRds a happy new year: yearSample
yearSample(2015)
# Have a nerdy
set.seed(1244); sample(0:9, 4, replace=TRUE)

}
}
\keyword{package}
\keyword{documentation}
