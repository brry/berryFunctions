# Berry Boessenkool, 2014
require2 <- library2 <- function(
                      name,
                      ...)
{
name <- as.character(substitute(name))
for(i in 1:length(name))
{
versuch <- try(library(name[i], character.only=TRUE, quietly=TRUE), silent=TRUE)
if(class(versuch)=="try-error")
   {
   install.packages(name, ...)
   require(name[i], character.only=TRUE)
   }
}
for(i in 1:length(name))
  message(paste0('-------------------------\nhelp(package="', name[i],
            '")\n-------------------------\n'))
}
