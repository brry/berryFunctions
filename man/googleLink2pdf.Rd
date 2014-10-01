\name{googleLink2pdf}
\alias{googleLink2pdf}
\title{extract pdf link from google search result}
\description{restrict pdf link from a google search to actual link with text processing}
\usage{googleLink2pdf(googlelink)}
\arguments{
  \item{googlelink}{Character: A search result address}
}
\value{Characterstring with only the basic link}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2012}
\note{The function is not vectorized! If you have many links, use a loop around this function...
Not tested for all that many links yet, please report erronous behaviour!}
\seealso{\code{\link{strsplit}}, \code{\link{gsub}}}
\examples{
Link <- paste0("http://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1",
        "&cad=rja&sqi=2&ved=0CDIQFjAA&url=http\%3A\%2F\%2Fcran.r-project.org",
        "\%2Fdoc\%2Fmanuals\%2FR-intro.pdf&ei=Nyl4UfHeOIXCswa6pIC4CA",
        "&usg=AFQjCNGejDwPlor4togQZmQEQv72cK9z8A&bvm=bv.45580626,d.Yms")
googleLink2pdf(Link)
}
\keyword{ character }
