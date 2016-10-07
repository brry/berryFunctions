
# Function development testing
# Berry Boessenkool, started Oct 2016


# smallPlot

owd <- setwd(desktop)
for(i in 1:2) {
  if(i==1) pdf("margtest_1.pdf") else pdf("margtest_2.pdf", height=5)
  plot(1:100, xlab=toString(letters))
  smallPlot(plot(5:1, ylab="Yo man!"), x1=0.0,x2=0.5, y1=0.2,y2=0.9, bg="lightblue")
  smallPlot(plot(5:1, ylab="Yo man-"), x1=0.0,x2=0.5, y1=0.2,y2=0.9, bg=addAlpha(2), outer=TRUE)
  smallPlot(plot(5:1, ylab="Yo man!"), x1=0.0,x2=0.7, y1=0.2,y2=0.5, bg="transparent")
  smallPlot(plot(5:1), x1=0.5,x2=  1, y1=0.5,y2=  1, bg="lightblue")
  smallPlot(plot(5:1), x1=0.5,x2=0.8, y1=0.5,y2=0.8, bg=addAlpha(2))
  dev.off()
}
setwd(owd)
rm(i,owd)


  
# par fig  vs   mtext

## with inital par reading

dev.off()
op1 <- par(no.readonly = TRUE)
plot(1:10)
mtext("hello", adj=1, col=2)            # written as expected  
par(fig=c(0.1,0.6,0.5,0.8), new=TRUE)
par(op1)
mtext("hello ", adj=1, col=3)           # not written
par(fig=c(0.1,0.8,0.3,0.8), new=TRUE)
plot(rnorm(400), type="l")
par(op1)
mtext("hello  ", adj=1, col=4)          # correct


## with mar

dev.off()
plot(1:10)
mtext("hello", adj=1, col=2)            # written as expected
op2 <- par(fig=c(0.1,0.6,0.5,0.8), mar=c(1,1,0,0), new=TRUE)   
par(op2)
mtext("hello ", adj=1, col=3)           # right spot
par(fig=c(0.1,0.8,0.3,0.8), mar=c(1,1,0,0), new=TRUE)
plot(rnorm(400), type="l")
par(op2)
mtext("hello  ", adj=1, col=4)          # relatively right spot


## with mar and initial par reading

dev.off()
op3 <- par(no.readonly = TRUE)                           
plot(1:10)
mtext("hello", adj=1, col=2)            # written as expected
par(fig=c(0.1,0.6,0.5,0.8), mar=c(1,1,0,0), new=TRUE)  
par(op3)
mtext("hello ", adj=1, col=3)           # not written
par(fig=c(0.1,0.8,0.3,0.8), mar=c(1,1,0,0), new=TRUE)
plot(rnorm(400), type="l")
par(op3)
mtext("hello  ", adj=1, col=4)          # right spot

op2
op3[c("fig","mar","new")] # seems the same
op1[c("fig","mar","new")]



## with on the fly reading, but no mar

dev.off()
plot(1:10)
mtext("hello", adj=1, col=2)            # written as expected
op4 <- par(fig=c(0.1,0.6,0.5,0.8), new=TRUE)   
par(op4)
mtext("hello ", adj=1, col=3)           # right spot
par(fig=c(0.1,0.8,0.3,0.8), new=TRUE)
plot(rnorm(400), type="l")
par(op4)
mtext("hello  ", adj=1, col=4)          # relatively right spot



op4







dev.off()
plot(1:10)
mtext("hello", adj=1, col=2)
smallPlot(plot(2), y1=0.5, y2=0.8)
par(mar=c(5,4,4,2))
mtext("hello", adj=1) # not working 
mtext("hello", adj=1, line=-1) # works
plot(1:10)
mtext("hello", adj=1, col=2)
smallPlot("")
mtext("hello", adj=1) # works
mtext("hello", adj=1, line=-1) # works
  