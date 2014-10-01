\name{triangleArea}
\alias{triangleArea}
\title{calculate Area of a triangle}
\description{calculate Area of a triangle}
\usage{triangleArea(x, y, digits = 3) }
\arguments{
  \item{x}{Vector with 3 values (x coordinates of triangle corners)}
  \item{y}{Ditto for y.}
  \item{digits}{Number of digits the result is rounded to. DEFAULT: 3)}
}
\value{Numeric}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2011}
\seealso{\code{\link{distance}} }
\examples{
a <- c(1,5.387965,9); b <- c(1,1,5)
plot(a[c(1:3,1)], b[c(1:3,1)], type="l", asp=1)#; grid()

triangleArea(a,b)
#triangleArea(a,b[1:2])

}
\keyword{spatial}
