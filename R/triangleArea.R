# Berry Boessenkool
triangleArea <- function(
   x, # Vector with 3 values (x coordinates of triangle corners)
   y, # Ditto for y.
   digits=3) # Number of digits the result is rounded to
{
if( !is.vector(x) | !is.vector(y) ) stop("Input must be a vector!")
if(length(x) != 3 | length(y) !=3 ) stop("Vectors must have 3 elements.")
A <- 0.5*(x[1] * (y[2] - y[3]) + x[2] * (y[3] - y[1]) + x[3] * (y[1] - y[2]))
return(round(abs(A),digits))
}
