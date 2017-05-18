
# Function development testing
# Berry Boessenkool, started Oct 2016

# colPointsLegend potentially extremely dependent on graphics size.
# may also require changes in smallPlot

# example comment to show off git


i <- c( 22,  40,  48,  60,  80,  70,  70,  63,  55,  48,  45,  40,  30,  32)
j <- c(  5,  10,  15,  20,  12,  30,  45,  40,  30,  36,  56,  33,  45,  23)
k <- c(175, 168, 163, 132, 120, 117, 110, 130, 131, 160, 105, 174, 190, 183)
 
for(hh in c(400,350,300))
{
png(paste0("test",hh,".png"), width=800, height=hh)
tryStack(colPoints(i,j,k, cex=1.5, pch="+", add=FALSE))
dev.off()
}
unlink(paste0("test",c(400,350,300),".png"))


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


  