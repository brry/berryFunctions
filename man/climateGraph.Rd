\name{climateGraph}
\alias{climateGraph}
\title{climate graph after Walter and Lieth}
\description{Draw a climate diagramm by the standards of Walter and Lieth.}
\usage{climateGraph(temp, rain,
  main="BerryStation\\n52\U00B0 24' N / 12\U00B0 58' E\\n42 m aSL",
  units=c("\U00B0 C", "mm"), labs=c("J","F","M","A","M","J","J","A","S","O","N","D"), 
  textprop=0.2, ylim=range(temp, rain/2), compress=FALSE, 
  ticks=-5:20*10, mar=c(1.5,2.3,4.5,2.3), box=TRUE, keeplayout=FALSE, 
  graylines=TRUE, lty=1, colrain="blue", coltemp="red", lwd=2, 
  arghumi=NULL, argarid=NULL, argcomp=NULL, ... )}
\arguments{
  \item{temp}{monthly temperature mean in degrees C}
  \item{rain}{monthly rain sum in mm (12 values)}
  \item{main}{location info as character string. can have \\n. DEFAULT: "BerryStation\\n52\\U00B0 24' N / 12\\U00B0 58' E\\n42 m aSL"}
  \item{units}{units used for labelling. DEFAULT: c("\\U00B0 C", "mm")}
  \item{labs}{labels for x axis. DEFAULT: c("J","F","M","A","M","J","J","A","S","O","N","D")}
  \item{textprop}{proportion of graphic that is used for writing the values in a table to the right. DEFAULT: 0.2}
  \item{ylim}{limit for y axis in temp units. DEFAULT: range(temp, rain/2)}
  \item{compress}{should rain>100 mm be compressed with adjusted labelling? (not recommended for casual visualization!). DEFAULT: FALSE}
  \item{ticks}{positions for vertical labelling and line drawing. DEFAULT: -5:20*10}
  \item{mar}{plot margins. DEFAULT: c(1.5,2.3,4.5,2.3)}
  \item{box}{draw box along outer margins of graph?. DEFAULT: TRUE}
  \item{keeplayout}{Keep the layout and parameters changed with par?. DEFAULT: FALSE}
  \item{}{NA}
  \item{graylines}{plot horizontal gray lines at every 10 degrees and vertically for each month?. DEFAULT: TRUE}
  \item{lty}{line type of gray lines, see \code{\link{par}}. DEFAULT: 1}
  \item{colrain}{Color for rain line and axis labels. DEFAULT: "blue"}
  \item{coltemp}{color for temperature line and axis labels. DEFAULT: "red"}
  \item{lwd}{line width of actual temp and rain lines. DEFAULT: 2}
  \item{arghumi}{Arguments for humid \code{\link{polygon}}, like density, angle. DEFAULT: NULL (internal x,y, col, border)}
  \item{argarid}{Arguments for arid area. DEFAULT: NULL}
  \item{argcomp}{Arguments for compressed rainfall polygon. DEFAULT: NULL}
  \item{\dots}{further arguments passed to plot, like col.main}
}
\value{None. Plots data and table.}
\references{Heinrich Walter, Helmut Lieth: Klimadiagramm-Weltatlas. Gustav Fischer Verlag, Jena 1967\cr
Examples:\cr
\url{http://www.hoelzel.at/_verlag/geojournal/archiv/klima/2006_01/lieth.gif}\cr
\url{http://www.hoelzel.at/_verlag/geojournal/archiv/klima/istanbul/istanbul200.gif}\cr
\url{http://www.ipb.uni-tuebingen.de/kurs/comp/1_excel2007/1_pic/2007diagramm_verbund02.jpg}\cr
\url{http://www.zivatar.hu/felhotar/albums/userpics/wldp.png}}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, June 2013}
\seealso{\code{diagwl} in package \code{climatol}}
\examples{
temp <- c(-9.3,-8.2,-2.8,6.3,13.4,16.8,18.4,17,11.7,5.6,-1,-5.9)#
rain <- c(46,46,36,30,31,21,26,57,76,85,59,46)

climateGraph(temp, rain) # default settings
climateGraph(temp, rain, textprop=0) # no table written to the right
climateGraph(temp, rain, lty=3) # dotted background lines
# vertical lines instead of filled polygon:
climateGraph(temp, rain, arghumi=list(density=15, angle=90)) 
# fill color for arid without transparency:
climateGraph(temp, rain, argarid=list(col="gold")) 
# for the Americans ;-) (axes should be different, though!):
climateGraph(temp, rain, units=c("\U00B0 F","in")) 

rain <- c(23, 11, 4, 2, 10, 53, 40, 15, 21, 25, 29, 22)
# fix ylim if you want to compare diagrams of different stations:
climateGraph(temp, rain, ylim=c(-15, 50)) # works with two arid phases as well

rain <- c(54, 23, 5, 2, 5, 70, 181, 345, 265, 145, 105, 80) # with extrema
climateGraph(temp, rain) # August can be visually compared to June
climateGraph(temp, rain, compress=TRUE) # compressing extrema enables a better
# view of the temperature, but heigths of rain cannot be visually compared anymore
climateGraph(temp, rain, compress=TRUE, ylim=c(-10, 90))
# needs ylim in linearly continued temp units
climateGraph(temp, rain, compress=TRUE, argcomp=list(density=30, col=6))

\dontrun{
## Rcmd check --as-cran doesn't like to open external devices such as pdf,
## so this example is excluded from running in the checks.
setwd("C:/Users/berry/Desktop")
pdf("ClimateGraph.pdf")
climateGraph(temp, rain, main="Another Station\nlocated somewhere\n369 ft a sl")
dev.off()

# further German reading:
browseURL("http://www.klimadiagramme.de/all.html")


# One large Dataset:
NOOAlink <- "http://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/"
browseURL(NOOAlink)
# Find your Station here:
browseURL(paste0(NOOAlink,"/station-inventories/allstations.txt")

# Data from Roseburg, Oregon, where I once lived:
download.file(destfile="Roseburg.txt", url=paste0("http://www1.ncdc.noaa.gov/",
          "pub/data/normals/1981-2010/products/station/USC00357331.normals.txt")
RT <- read.table(file="Roseburg.txt", skip=11, nrows=1, as.is=TRUE)[1,-1]
RT <- ( as.numeric(substr(RT,1,3))/10   - 32) * 5/9     # converted to degrees C
RP <- read.table(file="Roseburg.txt", skip=580, nrows=1, as.is=TRUE)[1,-1]
RP <-  as.numeric(substr(RP,1,nchar(RP)-1))/100*25.4
meta <- read.table(file="Roseburg.txt", nrows=5, as.is=TRUE, sep=":")
meta <- paste(meta[1,2], paste(meta[3:4 ,2], collapse=" /"), meta[5,2], sep="\n")

climateGraph(RT, RP, main=meta)
climateGraph(RT, RP, main=meta, compress=TRUE)


# abstract mean values from weather data

browseURL("http://www.dwd.de") # Klima Umwelt - Klimadaten - online,frei
# - Klimadaten Deutschland - Messstationen - Tageswerte

download.file(destfile="Potsdam.zip", url= paste(
"http://www.dwd.de/bvbw/generator/DWDWWW/Content/Oeffentlichkeit/KU/KU2/KU21/",
"klimadaten/german/download/tageswerte/kl__10379__hist__txt,templateId=raw,",
"property=publicationFile.zip/kl_10379_hist_txt.zip", sep=""))

unzip("Potsdam.zip", exdir="PotsdamKlima")
pk <- read.table(dir("PotsdamKlima", pattern="^[p]", full.names=TRUE), sep=";",
                 header=TRUE, na="-999")
dates <- strptime(pk$Mess_Datum, "\%Y\%m\%d")
temp <- tapply(pk$LUFTTEMPERATUR, INDEX=format(dates, "\%m"), FUN=mean, na.rm=FALSE)
precsums <- tapply(pk$NIEDERSCHLAGSHOEHE, INDEX=format(dates, "\%Y-\%m"), FUN=sum)
eachmonth <- format(strptime(paste(names(precsums),"01"), "\%Y-\%m \%d"),"\%m")
prec <- tapply(precsums, eachmonth, FUN=mean)
meta <- paste("Potsdam\n", paste(range(dates), collapse=" to "), "\n", sep="")

# If you want to add things later, use keeplayout and graphics.off() to reset par
climateGraph(temp, prec, main=meta, ylim=c(-2, 45), keeplayout=TRUE)
# Add Quartiles (as in boxplots): numerically sorted, 50\% of the data lie inbetween
T25 <- tapply(pk$LUFTTEMPERATUR, INDEX=format(dates, "\%m"),
              FUN=quantile, na.rm=FALSE, probs=0.25)
T75 <- tapply(pk$LUFTTEMPERATUR, INDEX=format(dates, "\%m"),
              FUN=quantile, na.rm=FALSE, probs=0.75)
arrows(x0=1:12, y0=T25, y1=T75, angle=90, code=3, col=2, len=0.1)
#
P25 <- tapply(precsums, eachmonth, FUN=quantile, na.rm=FALSE, probs=0.25)
P75 <- tapply(precsums, eachmonth, FUN=quantile, na.rm=FALSE, probs=0.75)
arrows(x0=1:12, y0=P25/2, y1=P75/2, angle=90, code=3, col=4, len=0, lwd=3, lend=1)
title(main=c("","","IQR shown als lines"), col.main=8)


# Comparison to diagrams in climatol
install.packages("climatol")
help(package="climatol")
library(climatol)
data(datcli)
diagwl(datcli,est="Example station",alt=100,per="1961-90",mlab="en")

}}
\keyword{hplot}
