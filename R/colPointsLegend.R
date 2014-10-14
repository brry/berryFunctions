# Berry Boessenkool,

# legend for colPoints
# Adds legends to plots created or enhanced with \code{\link{colPoints}}
# invisible list of par of smallPlot, adds legend bar to current plot
# \code{\link{colPoints}} for examples!

colPointsLegend <- function(
z, # Values of third dimension used in \code{\link{colPoints}}, can be matrix, vector, etc, but must be numeric
Range=range(z, finite=TRUE), # Ends of color bar for method=equalinterval
nbins=40, # Number of classes (thus, colors)
colors=rainbow2(nbins), # Color vector
bb=seqR(Range, length.out=nbins+1), # Borders of bins for the legend (key)
at=pretty2(Range), # Positions of legend labels
labels=at, # Labels that are written at the positions of \code{at}

bg="white", # Background behind key, labels and title
x1=60, y1=99, # Topleft relative coordinates (0:100) of inset plot, see \code{\link{smallPlot}}
x2=x1+38, y2=y1-11, # Bottomright -"-
mar, # Margins for \code{\link{smallPlot}} in relative values (0:100). DEFAULT: internal calculations based on title, labelpos and titlepos.
mgp=c(1.8, 0.6, 0), # MarGinPlacement: distance of xlab/ylab, numbers and line from plot margin, as in \code{\link{par}}, but with different defaults
sborder=NA, # Border around inset subplot
resetfocus=TRUE, # Reset focus to original plot? Specifies where further low level plot commands are directed to.

density=NULL, # Plot kernel density line? arguments passed to \code{\link{density}}
lines=TRUE, # Plot black lines in the color bar at \code{at}?
atminmax=FALSE, # Should the extrema of the legend be added to \code{at}?
horizontal=TRUE, # Horizontal bar? if FALSE, a vertical bar is drawn, ###with length and width exchanged
labelpos=1, # Position of labels relative to the bar. Possible: 1 (below), 2 (left), 3 (above), 4 (right), 5(on top of bar)
titlepos=3, # Position of title -"-
title="Legend", # Legend title
las=1, # LabelAxisStyle
...) # Further arguments passed to \code{\link{text}} and \code{\link{strwidth}}, e.g. cex, srt, font, col. But NOT adj!

{
# ------------------------------------------------------------------------------
z <- as.numeric(z)
# input checks:
if(any(diff(bb)<0)) stop("Breaks 'bb' (bin borders) have to be in ascending order.")
if(length(colors) != nbins) stop("Number of colors is not equal to number of classes.")
# extend labels and at:
if(atminmax) labels <- c( signif(head(bb,1),2), labels, signif(tail(bb,1),2) ) ### & length(labels)!=length(at)
if(atminmax) at <- c( head(bb,1), at, tail(bb,1) )
if(length(labels)!=length(at)) stop("labels and at do not have the same length")
# vertical default placement:
if(!horizontal){
if(missing(x1)) x1 <- 88
if(missing(y1)) y1 <- 70
if(missing(x2)) x2 <- x1+11
if(missing(y2)) y2 <- y1-40
if(missing(labelpos)) labelpos <- 2
if(missing(titlepos)) titlepos <- 3
if(missing(title)) title <- "Key"
}
# margin preparation:
if(missing(mar))
{
mar <- c(0,0,0,0)
wt <- 1.4*100*max( strwidth(c(labels, title), units="figure", ...))
ht <- 1.5*100*max(strheight(c(labels, title), units="figure", ...))
if(labelpos==2 | titlepos==2) mar[2] <- wt
if(labelpos==4 | titlepos==4) mar[4] <- wt
if(labelpos==1 | titlepos==1) mar[1] <- ht
if(labelpos==3 | titlepos==3) mar[3] <- ht
} # if mar is specified, it is used, of course.
# subplot setup:
smallPlot(x1=x1, y1=y1, x2=x2, y2=y2, mar=mar, mgp=mgp, bg=bg,
  border=sborder, las=las, resetfocus=resetfocus, expr={
if(horizontal) # ---------------------------------------------------------------
  {
  plot.window(xlim=c(bb[1], tail(bb,1)), ylim=c(0,1), xaxs="i", yaxs="i")
  # actually plot legend color fields:
  for(i in 1:length(colors))
    rect(xleft=bb[i], xright=bb[i+1], ybottom=0, ytop=1, col=colors[i], border=NA)
  # lines
  if(lines) segments(x0=at, y0=0, y1=1)
  # prepare label adjustment:
  if(labelpos==1) { y <- -0.1 ; vadj <- 1   } else
  if(labelpos==3) { y <-  1.1 ; vadj <- 0   } else
  if(labelpos==5) { y <-  0.5 ; vadj <- 0.5 } else
  stop("Wrong labelpos. Possible in horizontal legend: 1 (below legend bar), 3 (above), and 5 (on top).")
  # actually write labels:
  text(x=at, y=y, labels=labels, adj=c(0.5, vadj), xpd=TRUE, ...)
  # prepare title adjustment:
  pu <- par("usr")[1:2]
  if(titlepos==1) {x <- mean(pu); y <- -0.2; hadj <- 0.5; vadj <- 1   } else
  if(titlepos==2) {x <-    pu[1]; y <-  0.5; hadj <- 1  ; vadj <- 0.5 } else
  if(titlepos==3) {x <- mean(pu); y <-  1.2; hadj <- 0.5; vadj <- 0   } else
  if(titlepos==4) {x <-    pu[2]; y <-  0.5; hadj <- 0  ; vadj <- 0.5 } else
  if(titlepos==5) {x <- mean(pu); y <-  0.5; hadj <- 0.5; vadj <- 0.5 } else
  stop("Wrong titlepos. Must be integer between 1 and 5.")
  # actually write title:
  text(x=x, y=y, labels=title, adj=c(hadj, vadj), xpd=TRUE, ...)
  # kernel density:
  if(is.list(density) | is.null(density) | isTRUE(density) )
    {
    dp <- do.call(stats::density, args=owa(list(x=z, na.rm=TRUE), density))
    lines(dp$x, dp$y/max(dp$y))
    }
  }
else # if not horizontal, thus if vertical -------------------------------------
  {
  plot.window(ylim=c(bb[1], tail(bb,1)), xlim=c(0,1), xaxs="i", yaxs="i")
  # actually plot legend color fields:
  for(i in 1:length(colors))
    rect(ybottom=bb[i], ytop=bb[i+1], xleft=0, xright=1, col=colors[i], border=NA)
  # lines
  if(lines) segments(y0=at, x0=0, x1=1)
  # prepare label adjustment:
  if(labelpos==2) { x <- -0.1 ; hadj <- 1   } else
  if(labelpos==4) { x <-  1.1 ; hadj <- 0   } else
  if(labelpos==5) { x <-  0.5 ; hadj <- 0.5 } else
  stop("Wrong labelpos. Possible in vertical legend: 2 (left of legend bar), 4 (right), and 5 (on top).")
  # actually write labels:
  text(x=x, y=at, labels=labels, adj=c(hadj, 0.5), xpd=TRUE, ...)
  # prepare title adjustment:
  pu <- par("usr")[3:4]
  if(titlepos==1) {y <-    pu[1]; x <-  0.5; hadj <- 0.5; vadj <- 1  } else
  if(titlepos==2) {y <- mean(pu); x <- -0.2; hadj <- 1  ; vadj <- 0.5} else
  if(titlepos==3) {y <-    pu[2]; x <-  0.5; hadj <- 0.5; vadj <- -0.2} else
  if(titlepos==4) {y <- mean(pu); x <-  1.2; hadj <- 0  ; vadj <- 0.5} else
  if(titlepos==5) {y <- mean(pu); x <-  0.5; hadj <- 0.5; vadj <- 0.5} else
  stop("Wrong titlepos. Must be integer between 1 and 5.")
  # actually write title:
  text(x=x, y=y, labels=title, adj=c(hadj, vadj), xpd=TRUE, ...)
    # kernel density:
  if(is.list(density) | is.null(density) | isTRUE(density) )
    {
    dp <- do.call(stats::density, args=owa(list(x=z), density))
    lines(y=dp$x, x=dp$y/max(dp$y))
    }
  } # end vertical -------------------------------------------------------------
  })
}
