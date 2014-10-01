# Berry Boessenkool
smoothLines <- function(
    x,
    y,
    lwd=1,
    col=1,
    n=5,
    alpha=0.1,
    ...)
{
# Handling for Vector and Matrix with columns x and y
if(is.vector(x)) {x <- x; y <- y} else {y <- x[,2]; x <- x[,1]}
# extract RGB-values from color
k <- col2rgb(col)/255  ; R <- k[1,]; G <- k[2,]; B <- k[3,]
# plot transparent lines above each other
for(i in 1:n) {lines(x,y, col=rgb(R,G,B, alpha=alpha), lwd=lwd+n+1-i, ...) }
# add the original line above the rest
lines(x,y, col=col, lwd=lwd, ...)   
} # end of function
