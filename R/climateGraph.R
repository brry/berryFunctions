


# not run by roxygen because of unicode problems!



#' climate graph after Walter and Lieth
#' 
#' Draw a climate diagram by the standards of Walter and Lieth.
#' 
#' @return None. Plots data and table.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June 2013
#' @seealso \code{diagwl} in package \code{climatol}
#' @references Heinrich Walter, Helmut Lieth: Klimadiagramm-Weltatlas. Gustav Fischer Verlag, Jena 1967\cr
#' Examples:\cr
#' \url{https://www.hoelzel.at/__verlag/geojournal/archiv/klima/2006_01/lieth.gif}\cr
#' \url{https://www.hoelzel.at/__verlag/geojournal/archiv/klima/istanbul/istanbul400.gif}\cr
#' \url{http://www.ipb.uni-tuebingen.de/kurs/comp/1_excel2007/1_pic/2007diagramm_verbund02.jpg}\cr
#' \url{http://www.zivatar.hu/felhotar/albums/userpics/wldp.png}
#' @keywords hplot
#' @importFrom grDevices rgb
#' @importFrom graphics axis box layout lines mtext par plot text
#' @importFrom stats coef lm
#' @export
#' @examples
#' 
#' temp <- c(-9.3,-8.2,-2.8,6.3,13.4,16.8,18.4,17,11.7,5.6,-1,-5.9)#
#' rain <- c(46,46,36,30,31,21,26,57,76,85,59,46)
#' 
#' climateGraph(temp, rain)
#' climateGraph(temp, rain, textprop=0.6)
#' climateGraph(temp, rain, mar=c(2,3,4,3), textprop=0) # no table written to the right
#' # vertical lines instead of filled polygon:
#' climateGraph(temp, rain, arghumi=list(density=15, angle=90))
#' # fill color for arid without transparency:
#' climateGraph(temp, rain, argarid=list(col="gold"))
#' # for the Americans - axes should be different, though!:
#' climateGraph(temp, rain, units=c("\U{00B0}F","in"))
#' 
#' rain2 <- c(23, 11, 4, 2, 10, 53, 40, 15, 21, 25, 29, 22)
#' # fix ylim if you want to compare diagrams of different stations:
#' climateGraph(temp, rain2, ylim=c(-15, 50)) # works with two arid phases as well
#' 
#' op <- par(mfrow=c(2,1)) # mulipanel plot
#' climateGraph(temp, rain, argtext=list(cex=0.7))
#' climateGraph(temp, rain2, argtext=list(cex=0.7))
#' par(op)
#' 
#' rain <- c(54, 23, 5, 2, 5, 70, 181, 345, 265, 145, 105, 80) # with extrema
#' climateGraph(temp, rain) # August can be visually compared to June
#' climateGraph(temp, rain, compress=TRUE)
#' # compressing extrema enables a better view of the temperature,
#' # but heigths of rain cannot be visually compared anymore
#' climateGraph(temp, rain, compress=TRUE, ylim=c(-10, 90))
#' # needs ylim in linearly continued temp units
#' climateGraph(temp, rain, compress=TRUE, argcomp=list(density=30, col=6))
#' 
#' \dontrun{
#' pdf("ClimateGraph.pdf")
#' climateGraph(temp, rain, main="Another Station\nlocated somewhere else")
#' dev.off()
#' openFile(ClimateGraph.pdf")
#' unlink("ClimateGraph.pdf")
#' 
#' # further German reading:
#' browseURL("http://www.klimadiagramme.de/all.html")
#' 
#' 
#' # Climate Graphs for the USA:
#' NOOAlink <- "http://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/"
#' browseURL(NOOAlink)
#' # Find your Station here:
#' browseURL(paste0(NOOAlink,"/station-inventories/allstations.txt"))
#' 
#' # Data from Roseburg, Oregon:
#' download.file(destfile="Roseburg.txt", url=paste0("http://www1.ncdc.noaa.gov/",
#'           "pub/data/normals/1981-2010/products/station/USC00357331.normals.txt"))
#' RT <- read.table(file="Roseburg.txt", skip=11, nrows=1, as.is=TRUE)[1,-1]
#' RT <- ( as.numeric(substr(RT,1,3))/10   - 32) * 5/9     # converted to degrees C
#' RP <- read.table(file="Roseburg.txt", skip=580, nrows=1, as.is=TRUE)[1,-1]
#' RP <-  as.numeric(substr(RP,1,nchar(RP)-1))/100*25.4
#' meta <- read.table(file="Roseburg.txt", nrows=5, as.is=TRUE, sep=":")
#' meta <- paste(meta[1,2], paste(meta[3:4 ,2], collapse=" /"), meta[5,2], sep="\n")
#' unlink("Roseburg.txt")
#' 
#' climateGraph(RT, RP, main=meta)
#' climateGraph(RT, RP, main=meta, compress=TRUE)
#' 
#' 
#' # Climate Graphs for Germany:
#' browseURL("https://github.com/brry/rdwd#rdwd")
#' instGit("brry/rdwd")
#' link <- rdwd::selectDWD("Potsdam", res="monthly", var="kl", per="h")
#' clim <- rdwd::dataDWD(link, dir=tempdir())
#' dates <- strptime(clim$MESS_DATUM_BEGINN, "%Y%m%d")
#' temp <- tapply(clim$LUFTTEMPERATUR, INDEX=format(dates, "%m"), FUN=mean, na.rm=FALSE)
#' precsums <- tapply(clim$NIEDERSCHLAGSHOEHE, INDEX=format(dates, "%Y-%m"), FUN=sum)
#' eachmonth <- format(strptime(paste(names(precsums),"01"), "%Y-%m %d"),"%m")
#' prec <- tapply(precsums, eachmonth, FUN=mean)
#' meta <- paste("Potsdam\n", paste(range(dates, na.rm=TRUE), collapse=" to "), "\n", sep="")
#' 
#' climateGraph(temp, prec, main=meta, ylim=c(-2, 45))
#' # Add Quartiles (as in boxplots): numerically sorted, 50% of the data lie inbetween
#' TQ <- tapply(clim$LUFTTEMPERATUR, INDEX=format(dates, "%m"), FUN=quantile)
#' TQ <- sapply(TQ, I)
#' arrows(x0=1:12, y0=TQ["25%",], y1=TQ["75%",], angle=90, code=3, col=2, len=0.1)
#' #
#' PQ <- tapply(precsums, eachmonth, FUN=quantile)
#' PQ <- sapply(PQ, I)
#' arrows(x0=1:12, y0=PQ["25%",]/2, y1=PQ["75%",]/2, angle=90, code=3, col=4, len=0, lwd=3, lend=1)
#' mtext("IQR shown als lines", col=8, at=6.5, line=0.7, cex=1.2, font=2)
#' 
#' 
#' # Comparison to diagram in climatol
#' install.packages("climatol")
#' help(package="climatol")
#' library(climatol)
#' data(datcli)
#' diagwl(datcli,est="Example station",alt=100,per="1961-90",mlab="en")
#' 
#' }
#' 
#' @param temp monthly temperature mean in degrees C
#' @param rain monthly rain sum in mm (12 values)
#' @param main location info as character string. can have \\n.
#'            DEFAULT: "StatName\\n52d 24' N / 12d 58' E\\n42 m aSL"
#' @param units units used for labeling. DEFAULT: c("d C", "mm")
#' @param labs labels for x axis. DEFAULT: J,F,M,A,M,J,J,A,S,O,N,D
#' @param textprop proportion of graphic that is used for writing the values
#'                 in a table to the right. DEFAULT: 0.25
#' @param ylim limit for y axis in temp units. DEFAULT: range(temp, rain/2)
#' @param compress should rain>100 mm be compressed with adjusted labeling?
#'                 (not recommended for casual visualization!). DEFAULT: FALSE
#' @param ticklab positions for vertical labeling. DEFAULT: -8:30*10
#' @param ticklin positions for horizontal line drawing. DEFAULT: -15:60*5
#' @param box draw box along outer margins of graph? DEFAULT: TRUE
#' @param mar plot margins. DEFAULT: c(1.5,2.3,4.5,0.2)
#' @param keeppar Keep the changed graphical parameters? DEFAULT: TRUE
#' @param colrain Color for rain line and axis labels. DEFAULT: "blue"
#' @param coltemp color for temperature line and axis labels. DEFAULT: "red"
#' @param lwd line width of actual temp and rain lines. DEFAULT: 2
#' @param arghumi List of arguments for humid \code{\link{polygon}},
#'                like density, angle. DEFAULT: NULL (internal x,y, col, border)
#' @param argarid List of arguments for arid area. DEFAULT: NULL
#' @param argcomp List of arguments for compressed rainfall polygon. DEFAULT: NULL
#' @param arggrid List of arguments for background grid lines. DEFAULT: NULL
#' @param argtext List of arguments for text at right hand if textprop>0. DEFAULT: NULL
#' @param \dots further arguments passed to plot, like col.main
#' 
climateGraph <- function(
     temp,
     rain,
     main="StatName\n52\U{00B0}24' N / 12\U{00B0}58' E\n42 m aSL",
     units=c("\U{00B0}C", "mm"),
     labs=substr(month.abb,1,1),
     textprop=0.25,
     ylim=range(temp, rain/2),
     compress=FALSE,
     ticklab= -8:30*10,
     ticklin=-15:60*5,
     box=TRUE,
     mar=c(1.5,2.3,4.5,0.2),
     keeppar=TRUE,
     colrain="blue",
     coltemp="red",
     lwd=2,
     #colcomp="purple", # color for compressed polygon ##### or in argcomp?
     arghumi=NULL,
     argarid=NULL,
     argcomp=NULL,
     arggrid=NULL,
     argtext=NULL,
     ...
     )
{
# function start ---------------------------------------------
# input checking:
if(length(temp)!=12 | length(rain)!=12) stop("temp and rain each need to have 12 elements.")
if(textprop>0.99) stop("textprop (",textprop,") is too large, must be <0.99")
# prepare plot, write table of values at the right:
rainsum <- sum(rain) # must be calculated before compression
if(compress)
  {
  # compress all rain above 100 mm
  rain[rain>100] <- rain[rain>100]*0.2 + 80
  # new ylim
  if(missing(ylim)) ylim <- range(temp, rain/2)
  }
# set margins around plot, avoid empty space along x-axis
op <- par(mar=mar, mgp=c(3,0.8,0) )
if(!keeppar) on.exit(par(op))
xlim <- xlim2 <- c(0.6, 12.4)
if(textprop > 0) xlim2[2] <- diff(xlim2)/(1-textprop)+xlim2[1]
# Empty plot:
plot(1, type="n", xlim=xlim2, ylim=ylim, xaxs="i", axes=FALSE, ann=FALSE, ...)
mtext(main, side=3, at=6.5, line=1, cex=1.2, font=2)
# background lines:
for(t in ticklin) do.call(lines, owa(list(x=xlim, y=rep(t,2), col=8), arggrid) )
lines(xlim, c(0,0))
do.call(abline, owa(list(v=1:11+0.5, col=8), arggrid) )
if(box)
  {
  lines(xlim, rep(par("usr")[3],2))
  lines(xlim, rep(par("usr")[4],2))
  lines(rep(xlim[2],2), c(-100,500))
  }

# determine arid and humid times: ---------------------------------------------------
# determine interception months: (each before the actual interception):
intm <- which(diff(rain/2>temp) != 0 )
if(length(intm) >0 )
  {
  # interception coordinates:
  intc <- sapply(intm, function(i) {
    # coefficients of straight line between two points:
    Ct <- coef(lm(temp[i+0:1]   ~ c(i+0:1) )) # temp = a + b*x
    Cr <- coef(lm(rain[i+0:1]/2 ~ c(i+0:1) )) # rain = c + d*x
    # both are equal at crossing point x -> a+bx=c+dx -> bx-dx = c-a -> x = (c-a)/(b-d)
    x <- (Cr[1]-Ct[1]) / (Ct[2]-Cr[2])
    # temp crosses zero at a + b*x = 0 -> x=-a/b
    as.vector(c(x, Ct[1] + x*Ct[2] )) # return of each sapply run: x and y coordinates
    })
  # prepare polygon drawing positions
  px <- c(1:12, intc[1,]) # polygon x coordinates (unsorted)
  tpy <- c(temp,   intc[2,])[order(px)] # temp polygon y coordinates
  rpy <- c(rain/2, intc[2,])[order(px)] # rain -"-
  px <- sort(px)
  } else # if there are no interceptions of the two lines:
  {
  px <- 1:12
  tpy <- temp  # temp polygon y coordinates
  rpy <- rain/2  # all in temp units
  }

# polygon drawing - ARID ------------------------------------------------------------
arid <- which(rpy<=tpy)
argarid_def <- list(x=px[c(arid, rev(arid))], y=c(rpy[arid],rev(tpy[arid])),
                    col=rgb(1,0.84,0, alpha=0.3), border=NA) # col from col2rgb("gold")/255
do.call(polygon, args=owa(d=argarid_def, a=argarid, "x","y")  )

# polygon drawing - HUMID ------------------------------------------------------------
# interception coordinates of temp with 0-axis (baseline of humid polygon):
intc_t <- sapply(  which(diff(temp>0) != 0)  , function(i) {
    Ct <- coef(lm(temp[i+0:1]   ~ c(i+0:1) )) # temp = a + b*x crosses zero at a + b*x = 0 -> x=-a/b
    as.vector( c(-Ct[1]/Ct[2], 0) ) }) # return of each sapply run: x coordinates
tpy[tpy<0] <- 0
isneg <- length(intc_t) > 0 # is there any negative temperature
###humid <- which(rpy>=tpy)
hpx <- c( px, if(isneg) intc_t[1,] ) # backwards polygon border
hpy <- c(tpy, if(isneg)intc_t[2,] )[order(hpx, decreasing=TRUE)]
hpx <- sort(hpx, decreasing=TRUE)
rpy[rpy<tpy] <- tpy[rpy<tpy] # have the polygon go along templine, so density starting lines are overplotted later
arghumi_def <- list(x=c(px, hpx), y=c(rpy, hpy), col=rgb(0,0,1, alpha=0.3), border=NA)
do.call(polygon, args=owa(d=arghumi_def, a=arghumi, "x","y")  )

# polygon drawing - compressed area -----------------------------------------------------
if(compress & sum(diff(rain>100) !=0) >0 )
{
# interception coordinates of rain with 1000-axis (baseline of compressed polygon):
intc_c <- sapply(  which(diff(rain>100) != 0)  , function(i) {
    Cc <- coef(lm(rain[i+0:1]   ~ c(i+0:1) )) # rpy = a + b*x = 100 -> x=(100-a)/b
    as.vector( c((100-Cc[1])/Cc[2], 50) ) }) # return of each sapply run: x and y coordinates
cpx <- c( px, intc_c[1,] ) # backwards polygon border
cpy <- c(rpy, intc_c[2,] )[order(cpx, decreasing=FALSE)]
cpx <- sort(cpx, decreasing=FALSE)
argcomp_def <- list(x=c(cpx, rev(cpx)), y=c(cpy,pmin(rev(cpy),50)), col=rgb(1,0,1, alpha=0.3), border=NA)
do.call(polygon, args=owa(d=argcomp_def, a=argcomp, "x","y")  )
}

# lines and labels ----------------------------------------------------------------
# plot temp line:
lines(temp, col=coltemp, type="l", lwd=lwd)
# plot rain line:
lines(rain/2, col=colrain, type="l", lwd=lwd)
# text block:
if(textprop > 0)
  {
  xpos <- xlim[2] + (xlim2[2]-xlim[2])/4*2:4-0.3
  ypos <- rep(mean(ylim),3)
  column1 <- paste(c(" m \n", "----", labs), collapse="\n")
  column2 <- paste(c(" T ",units[1], "----", sprintf("%4.1f", round(temp,1))), collapse="\n")
  column3 <- paste(c(" P ",units[2], "----", sprintf("%4.0f", round(rain)  )), collapse="\n")
  argtextdef <- list(x=xpos, y=ypos, labels=c(column1,column2,column3), adj=1, xpd=TRUE)
  do.call(text, owa(argtextdef, argtext, "x","y","labels"))
  }
# labeling:
mtext( paste("\U00D8", round(mean(temp),1), units[1]),        side=3, col=coltemp, line=1,   adj=0.01)
mtext(bquote(sum()* " "*.(round(rainsum,1))*" "*.(units[2])), side=3, col=colrain, line=0.8, adj=1.08, at=xlim[2])
axis(side=2, at=ticklab, col.axis=coltemp, las=1)
if(compress) ticklab <- ticklab[ticklab<=50]
axis(side=4, at=ticklab[ticklab>=0], ticklab[ticklab>=0]*2, col.axis=colrain, pos=xlim[2], las=1)
if(compress) axis(4, 6:9*10, 6:9*100-400, col.axis=owa(argcomp_def, argcomp)$col, pos=xlim[2], las=1)
axis(1, 1:12, labs, mgp=c(3,0.3,0), tick=FALSE)
} # end of function

