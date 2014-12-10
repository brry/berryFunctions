# Klimadiagramm - climate graph after Walter and Lieth
# Berry Boessenkool, June 2013   berry-b@gmx.de   Feedback is very welcome!

climateGraph <- function(
     temp, # monthly temperature mean in degrees C
     rain, # monthly rain sum in mm (12 values)
     main="BerryStation\n52\U00B0 24' N / 12\U00B0 58' E\n42 m aSL", # location info as character string. can have \n
     units=c("\U00B0 C", "mm"), # units used for labelling
     labs=c("J","F","M","A","M","J","J","A","S","O","N","D"), # labels for x axis
     textprop=0.2, # proportion of graphic that is used for writing the values in a table to the right
     ylim=range(temp, rain/2), # limit for y axis in temp units
     compress=FALSE, # should rain>100 mm be compressed with adjusted labelling? (not recommended for casual visualization!)
     ticks=-5:20*10,# positions for vertical labelling and line drawing
     mar=c(1.5,2.3,4.5,2.3), # plot margins
     box=TRUE, # draw box along outer margins of graph?
     keeplayout=FALSE, # Keep the layout and parameters changed with par?
     graylines=TRUE, # plot horizontal gray lines at every 10 degrees and vertically for each month?
     lty=1, # line type of gray lines, see ?par
     colrain="blue", # Color for rain line and axis labels
     coltemp="red", # color for temperature line and axis labels
     lwd=2, # line width of actual temp and rain lines
     #colcomp="purple", # color for compressed polygon ##### or in argcomp?
     arghumi=NULL, # Arguments for humid polygon
     argarid=NULL, # Arguments for arid area
     argcomp=NULL, # Arguments for compressed rainfall polygon
     ... # further arguments passed to plot, like col.main
     )
{
# function start ---------------------------------------------
# input checking:
if(length(temp)!=12 | length(rain)!=12) stop("temp and rain each need to have 12 elements.")
# prepare plot, write table of values at the right:
if(textprop > 0)
  {
  layout(matrix(2:1, ncol=2), widths=c(1-textprop, textprop))
  op <- par(mar=rep(0,4))
  plot(1:9, type="n", ann=FALSE, axes=FALSE)
  text(2.8, 5, paste(c(" m \n", "----", labs), collapse="\n"), adj=1 )
  text(5.8, 5, paste(c(" T ",units[1], "----", sprintf("%4.1f", round(temp,1))), collapse="\n"), adj=1 )
  text(8.8, 5, paste(c(" P ",units[2], "----", sprintf("%4.0f", round(rain)  )), collapse="\n"), adj=1 )
  if(box) box()
  par(op)
  }
if(compress)
  {
  # compress all rain above 100 mm
  rain[rain>100] <- rain[rain>100]*0.2 + 80
  # new ylim
  if(missing(ylim)) ylim <- range(temp, rain/2)
  }
# set margins around plot, avoid empty space along x-axis
op <- par(mar=mar, mgp=c(3,0.8,0), xaxs="i") 
# Empty plot:
plot(1, type="n", xlim=c(0.6, 12.4), ylim=ylim, main=main, xaxt="n", yaxt="n", ylab="", xlab="", ...)
# background lines:
if(graylines)  abline(h=ticks, v=1:11+0.5, col=8, lty=lty) # h=pretty(ylim)
abline(h=0)

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
# labelling:
mtext(paste("\U00D8", round(mean(temp),1), units[1]),           side=3, col=coltemp, line=-2.0, adj=0.02, outer=T)
mtext(bquote(sum()* " "*.(round(sum(rain),1))*" "*.(units[2])), side=3, col=colrain, line=-2.2, adj=1.08, outer=T, at=par("fig")[2])
if(compress) ticks <- ticks[ticks<=50]
axis(side=2, at=ticks, col.axis=coltemp, las=1)
axis(side=4, at=ticks[ticks>=0], ticks[ticks>=0]*2, col.axis=colrain, las=1)
if(compress) axis(4, 6:9*10, 6:9*100-400, col.axis=owa(argcomp_def, argcomp)$col, las=1)
axis(1, 1:12, labs, mgp=c(3,0.3,0), tick=FALSE)
box() # cover up gray lines on top of original box
if(box) box("outer") # draw box along outer margins of graph
if(!keeplayout)
  {
  par(op) # set old margins again
  layout(matrix(1)) # set old layout again
  }
} # end of function

