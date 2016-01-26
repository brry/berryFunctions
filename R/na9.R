# na strings with varying amount of spaces ahead
# Berry Boessenkool, Jan 2016, berry-b@gmx.de

na9 <- function(
nspace=5, # number of spaces prepended
base=c("-9999","-999", "-99", "-9.99", "-9.9", "-9,99", "-9,9"), # basic na.string structures. Might be expanded in the future. Thankful for suggestions.
more="", # allows more structures added to base
...) # Arguments passed to nothing currently
{
base <- c(base, more)
spaces <- sapply(0:nspace, function(i) paste(rep(" ",i), collapse=""))
paste0(rep(spaces, each=length(base)), base)
}
