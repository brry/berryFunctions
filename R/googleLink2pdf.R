#' extract pdf link from google search result
#' 
#' restrict pdf link from a google search to actual link with text processing
#' 
#' @return Characterstring with only the basic link
#' @note The function is not vectorized! If you have many links, use a loop around this function... 
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2012
#' @seealso \code{\link{strsplit}}, \code{\link{gsub}}
#' @keywords character
#' @export
#' @examples
#' 
#' Link <- paste0("http://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1",
#'         "&cad=rja&sqi=2&ved=0CDIQFjAA&url=http%3A%2F%2Fcran.r-project.org",
#'         "%2Fdoc%2Fmanuals%2FR-intro.pdf&ei=Nyl4UfHeOIXCswa6pIC4CA",
#'         "&usg=AFQjCNGejDwPlor4togQZmQEQv72cK9z8A&bvm=bv.45580626,d.Yms")
#' googleLink2pdf(Link)
#' 
#' Link <- paste0("https://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1",
#'         "&cad=rja&uact=8&ved=0ahUKEwjLlfmClavRAhWaN1AKHcGSBjEQFgghMAA",
#'         "&url=http%3A%2F%2Fstackoverflow.com%2Fquestions%2Ftagged%2Fr",
#'         "&usg=AFQjCNHYj6HjSs6Lvczn9wMWxE3slCdq1Q&bvm=bv.142059868,d.ZWM")
#' googleLink2pdf(Link)
#' Link <- paste0("https://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2",
#'         "&cad=rja&uact=8&ved=0ahUKEwjLlfmClavRAhWaN1AKHcGSBjEQFggpMAE&",
#'         "url=http%3A%2F%2Fstackoverflow.com%2Fquestions%2Ftagged%2F%3Ftagnames",
#'         "%3Dr%26sort%3Dactive&usg=AFQjCNGkPGHq05qwKLLW4vRXdmk2Olhmig&bvm=bv.142059868,d.ZWM")
#' googleLink2pdf(Link)
#' 
#' @param googlelink Character string: A search result address
#' 
googleLink2pdf <- function(
googlelink
)
{
pdflink <- strsplit(googlelink, "&url=")[[1]][2]
pdflink <- strsplit(pdflink, "&ei=")[[1]][1]
pdflink <- strsplit(pdflink, "&usg=")[[1]][1]
# printable ascii characters from http://www.ascii-code.com/
hex <- "20 21 22 23 24 25 26 27 28 29 2A 2B 2C 2D 2F 3A 3B 3C 3D 3E 3F 40"
ascii <- "! \" # $ % & ' ( ) * + , - / : ; < = > ? @"
hex <- paste0("%",strsplit(hex, " ")[[1]])
ascii <- c(" ",strsplit(ascii, " ")[[1]])
for(i in seq_along(hex)) pdflink <- gsub(hex[i], ascii[i], pdflink)
pdflink
}
