horizHist <- function(
   Data,
   breaks="Sturges",
   freq=TRUE,
   plot=TRUE,
   col=par("bg"),
   border=par("fg"),
   las=1, 
   xlab=if(freq)"Frequency" else "Density", 
   main=paste("Histogram of",deparse(substitute(Data))),
   ylim=range(HBreaks),
   labelat=pretty(ylim),
   labels=labelat,
   ... )
{
a <- hist(Data, plot=FALSE, breaks=breaks)
HBreaks <- a$breaks
HBreak1 <- a$breaks[1]
hpos <- function(Pos) (Pos-HBreak1)*(length(HBreaks)-1)/ diff(range(HBreaks))
if(plot) 
   {
   barplot(if(freq)a$counts else a$density, space=0, horiz=TRUE, ylim=hpos(ylim), col=col, border=border, 
        xlab=xlab, main=main, ...)      
   axis(2, at=hpos(labelat), labels=labels, las=las, ...) 
   }
hpos # return(invisible(hpos))
} # End of function
