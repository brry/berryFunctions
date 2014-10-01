# yearSample:
# Nerdy way to wish someone a happy new year, eg:
# Have a great
# set.seed(1244); sample(0:9, 4, replace=T)

# Berry Boessenkool, berry-b@gmx.de, 15 April 2014


yearSample <- function(
    year
    )
{
year2 <- as.numeric(substring(year, first=1:4, last=1:4))
year_is_false <- function(i)
  {
  set.seed(i)
  any(  sample(0:9, 4, replace=TRUE) != year2  )
  }
i <- 0
while( year_is_false(i) ) i <- i+1
output <- paste0("set.seed(", i, "); sample(0:9, 4, replace=T)\n")
cat(output)
}

