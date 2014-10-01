# nameSample:
# Find the seed necessary to produce a character sequence by using sample
# can especially impress newbies a lot.

# Berry Boessenkool, berry-b@gmx.de, 15 April 2014


nameSample <- function(
                      name
                      )
{
nc <- nchar(name)
# input checking:
if(nc>5)
  {
  answer <- readline("This will take very long. Sure you want to continue? y/n ")
  if(answer =="n") stop("Function cancelled.")
  }
seed_is_false <- function(i)
  {
  set.seed(i)
  paste( sample(letters, nc, replace=TRUE), collapse="") != name
  }
i <- 0
while( seed_is_false(i) ) i <- i+1
output <- paste0("set.seed(", i, ")\npaste(sample(letters, ", nc,
                 ", replace=T), collapse='')\n")
cat(output)
}

