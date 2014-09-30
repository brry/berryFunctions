# Table with values with value-dependent colored backgrounds in pdf
# 2012-11-13

# I saw a presentation today with a table of values of differences between several
# models and datasets of global precipitation. I decided I don't like reading
# 20+ values, and would like to see a corresponding color in the background of each cell. (heatmap)
# Writing this function took me about 1 hour and 30 minutes and was a nice brain excercise.
# Feedback welcome at berry-b@gmx.de!

tableColVal <- function(
   mat,
   pdffile="table_col_val.pdf",
   pdf=FALSE,
   nameswidth=0.3,   # percentage of plot
   namesheight=0.1,
   palette=rainbow(nrow(mat)*ncol(mat), start=0, end=0.7),
   Range=range(mat,finite=TRUE),
   argrow=NULL,
   argcol=NULL,
   argcell=NULL,
   argmain=NULL,
   ...)
{
# expand pdf-path to working directory if only file name (without path) is given:
if(pdf){   if(!grepl("/", pdffile) | grepl("\\", pdffile, fixed=TRUE) )
               pdffile <- paste(getwd(), pdffile, sep="/")  #"
        pdf(pdffile, ...) }# open pdf device
mat <- as.matrix(mat)
nc <- ncol(mat) ; nr <- nrow(mat)
# set plot
op <- par(mai=c(0, 0, namesheight*par()$pin[2], 0), xpd=TRUE )
plot(1, ylim=c(nr+1, 1), xlim=c(0,1), type="n", xaxs="i", yaxs="i", axes=FALSE, ann=FALSE)
# set positions for text and lines
rights <- seq(nameswidth, 1, len=nc+1)
lefts <- c(0, rights[1:nc] )
middles <- nameswidth + (1:nc*2-1) * (1-nameswidth)/nc/2
# define color for each value of mat
mod <- lm(c(1, length(palette)) ~ Range)[[1]]
lincol <- round(as.vector(mat) * mod[2] + mod[1])
# plot rectancles with colors corresponding to values of mat
rect(xleft=rep(lefts[-1], each=nr), xright=rep(rights[-1], each=nr),
     ybottom=rep(2:(nr+1), nc), ytop=rep(1:nr, nc), col=palette[lincol] , border=NA)
abline(v=rights, h=1:nr)
# add "titles"
ytitles <- 1-(namesheight*nr/2)
do.call(text, args=owa(d=list(x=middles,      y=ytitles,  labels=colnames(mat)), argcol,  u=c("x","y")))
do.call(text, args=owa(d=list(x=nameswidth/2, y=ytitles,  labels="tableColVal"), argmain, u=c("x","y")))
do.call(text, args=owa(d=list(x=nameswidth/2, y=1:nr+0.5, labels=rownames(mat)), argrow,  u=c("x","y")))
# add text to each cell
do.call(text, args=owa(d=list(x=rep(middles, each=nr), y=rep(1:nr, nc)+0.5, 
                              labels=as.vector(mat)), argcell,  u=c("x","y", "labels")))
# Set old paramaters again:
par(op)
if(pdf) { dev.off() ; cat("PDF-File is located here:", pdffile, "\n") }# close pdf device
} # end of function

