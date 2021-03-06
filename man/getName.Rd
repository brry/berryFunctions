% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getName.R
\name{getName}
\alias{getName}
\title{get the name of an input in nested function calls}
\usage{
getName(x)
}
\arguments{
\item{x}{input object name or character string}
}
\value{
Character string with the name
}
\description{
get the name of an input in nested function calls
}
\examples{
# This does not work well:

lower <- function(x) deparse(substitute(x))
upper <- function(y) lower(y)
lower(pi) # returns "pi", as expected
upper(pi) # returns "y".

# That's why there is getName:

getName(pi) # returns "pi", as expected
upper <- function(y) getName(y)
upper(pi) # yay!

upper("dummy")
upper(dummy) # works also for nonexistent objects
dummy <- 7
upper("dummy") # still stable
upper(dummy) # still stable

upper(stackloss[1:5,])

upper2 <- function(data) upper(data)
upper2("K")
upper2(K)

# getName only works correctly if x is not an evaluated object:
lower2 <- function(inp, assign=FALSE) {if(assign) inp <- inp; getName(inp)}
lower2(pi)       # "pi"
lower2(pi, TRUE) # "3.14159265358979"

}
\seealso{
\url{https://stackoverflow.com/a/26558733}, \code{\link{substitute}}
}
\author{
\url{https://stackoverflow.com/users/2725969/brodieg}
        Implementation Berry Boessenkool, \email{berry-b@gmx.de}, Sep 2016
}
\keyword{IO}
\keyword{character}
